use std::collections::{HashMap, HashSet};

use egui::{Color32, CornerRadius, FontId, Painter, Pos2, Rect, Stroke, StrokeKind, Vec2};

use crate::api::ServerLocation;
use crate::world_data::{self, LandGrid, Region};

// Colors matching the dark command-center aesthetic
const BG_COLOR: Color32 = Color32::from_rgb(10, 12, 18);
const LAND_DOT: Color32 = Color32::from_rgb(120, 120, 120);
const MAP_BORDER: Color32 = Color32::from_rgb(36, 40, 52);
const SERVER_ACTIVE: Color32 = Color32::from_rgb(60, 200, 60);
const SERVER_BLOCKED: Color32 = Color32::from_rgb(220, 50, 50);
const SERVER_HOVERED: Color32 = Color32::from_rgb(255, 230, 50);
const LABEL_BG: Color32 = Color32::from_rgba_premultiplied(20, 22, 30, 220);
const LABEL_BORDER: Color32 = Color32::from_rgb(50, 55, 65);
const LABEL_TEXT: Color32 = Color32::from_rgb(200, 205, 215);
const LABEL_DIM: Color32 = Color32::from_rgb(120, 125, 135);
const BLOCKED_TEXT: Color32 = Color32::from_rgb(220, 70, 70);

pub struct MapState {
    pub offset: Vec2,
    pub zoom: f32,
    pub land_grid: LandGrid,
}

impl Default for MapState {
    fn default() -> Self {
        Self {
            offset: Vec2::ZERO,
            zoom: 1.0,
            land_grid: world_data::generate_land_grid(),
        }
    }
}

/// Convert geographic coordinates to screen position
fn geo_to_screen(lon: f64, lat: f64, map_rect: Rect, state: &MapState) -> Pos2 {
    let norm_x = ((lon + 180.0) / 360.0) as f32;
    let norm_y = ((90.0 - lat) / 180.0) as f32;

    let center = map_rect.center();
    let w = map_rect.width() * state.zoom;
    let h = map_rect.height() * state.zoom;

    Pos2::new(
        center.x + state.offset.x + (norm_x - 0.5) * w,
        center.y + state.offset.y + (norm_y - 0.5) * h,
    )
}

/// Convert screen position back to geographic coordinates
fn screen_to_geo(pos: Pos2, map_rect: Rect, state: &MapState) -> (f64, f64) {
    let center = map_rect.center();
    let w = map_rect.width() * state.zoom;
    let h = map_rect.height() * state.zoom;

    let norm_x = (pos.x - center.x - state.offset.x) / w + 0.5;
    let norm_y = (pos.y - center.y - state.offset.y) / h + 0.5;

    let lon = (norm_x as f64) * 360.0 - 180.0;
    let lat = 90.0 - (norm_y as f64) * 180.0;
    (lon, lat)
}

/// Draw the dot-matrix world map
pub fn draw_map(painter: &Painter, map_rect: Rect, state: &MapState) {
    // Dark background
    painter.rect_filled(map_rect, 0.0, BG_COLOR);
    painter.rect_stroke(
        map_rect,
        CornerRadius::ZERO,
        Stroke::new(1.0, MAP_BORDER),
        StrokeKind::Outside,
    );

    // Compute dot spacing based on zoom
    let base_spacing = 8.0_f32;
    let spacing = base_spacing;
    let dot_radius = 1.2 * state.zoom.sqrt();

    // Draw land dots in a grid pattern
    let mut x = map_rect.left();
    while x < map_rect.right() {
        let mut y = map_rect.top();
        while y < map_rect.bottom() {
            let (lon, lat) = screen_to_geo(Pos2::new(x, y), map_rect, state);

            // Skip out-of-bounds coordinates
            if lon >= -180.0 && lon <= 180.0 && lat >= -90.0 && lat <= 90.0 {
                let val = world_data::land_value(&state.land_grid, lon, lat);
                if val > 0.3 {
                    let alpha = (val * 0.8).min(1.0);
                    let color = Color32::from_rgba_premultiplied(
                        (LAND_DOT.r() as f32 * alpha) as u8,
                        (LAND_DOT.g() as f32 * alpha) as u8,
                        (LAND_DOT.b() as f32 * alpha) as u8,
                        (255.0 * alpha) as u8,
                    );
                    painter.circle_filled(Pos2::new(x, y), dot_radius, color);
                }
            }
            y += spacing;
        }
        x += spacing;
    }
}

/// Compute region statistics
pub fn compute_region_stats(
    servers: &[ServerLocation],
    blocked: &HashSet<String>,
) -> HashMap<Region, (usize, usize)> {
    let mut stats: HashMap<Region, (usize, usize)> = HashMap::new();
    for server in servers {
        let region = world_data::classify_region(server.longitude, server.latitude, &server.desc);
        let entry = stats.entry(region).or_insert((0, 0));
        entry.0 += 1;
        if blocked.contains(&server.code) {
            entry.1 += 1;
        }
    }
    stats
}

/// Draw region label boxes on the map
pub fn draw_region_labels(
    painter: &Painter,
    map_rect: Rect,
    state: &MapState,
    stats: &HashMap<Region, (usize, usize)>,
) {

    for &region in Region::all() {
        let (total, blocked_count) = stats.get(&region).copied().unwrap_or((0, 0));
        if total == 0 {
            continue;
        }

        let (lon, lat) = region.label_pos();
        let center = geo_to_screen(lon, lat, map_rect, state);

        // Skip if off-screen
        if !map_rect.expand(50.0).contains(center) {
            continue;
        }

        let name = region.label();
        let status_text = if blocked_count > 0 {
            format!("{}/{} blocked", blocked_count, total)
        } else {
            format!("{} servers", total)
        };

        // Measure text for box sizing
        let name_font = FontId::proportional(13.0);
        let status_font = FontId::proportional(10.0);

        let name_galley = painter.layout_no_wrap(name.to_string(), name_font.clone(), LABEL_TEXT);
        let status_galley = painter.layout_no_wrap(
            status_text.clone(),
            status_font.clone(),
            if blocked_count > 0 {
                BLOCKED_TEXT
            } else {
                LABEL_DIM
            },
        );

        let text_width = name_galley.size().x.max(status_galley.size().x);
        let text_height = name_galley.size().y + status_galley.size().y + 2.0;

        let padding = Vec2::new(10.0, 6.0);
        let box_size = Vec2::new(text_width + padding.x * 2.0, text_height + padding.y * 2.0);
        let box_rect = Rect::from_center_size(center, box_size);

        // Draw box background
        painter.rect_filled(box_rect, CornerRadius::same(4), LABEL_BG);
        painter.rect_stroke(
            box_rect,
            CornerRadius::same(4),
            Stroke::new(1.0, LABEL_BORDER),
            StrokeKind::Outside,
        );

        // Draw name text (centered)
        let name_pos = Pos2::new(
            box_rect.center().x - name_galley.size().x / 2.0,
            box_rect.top() + padding.y,
        );
        painter.galley(name_pos, name_galley, Color32::PLACEHOLDER);

        // Draw status text (centered below name)
        let status_pos = Pos2::new(
            box_rect.center().x - status_galley.size().x / 2.0,
            box_rect.top() + padding.y + 15.0,
        );
        painter.galley(status_pos, status_galley, Color32::PLACEHOLDER);
    }
}

/// Draw server location dots and return the index of the hovered server (if any)
pub fn draw_server_dots(
    painter: &Painter,
    map_rect: Rect,
    state: &MapState,
    servers: &[ServerLocation],
    blocked: &HashSet<String>,
    mouse_pos: Option<Pos2>,
) -> Option<usize> {
    let dot_radius = (5.0 * state.zoom).clamp(4.0, 14.0);
    let hover_threshold = dot_radius + 8.0;

    let mut points = Vec::new();
    for (i, server) in servers.iter().enumerate() {
        let pos = geo_to_screen(server.longitude, server.latitude, map_rect, state);
        if !map_rect.expand(20.0).contains(pos) {
            continue;
        }
        points.push((i, pos, blocked.contains(&server.code)));
    }

    let mut hovered_idx = None;
    if let Some(mp) = mouse_pos {
        let mut min_dist = hover_threshold;
        for (i, pos, _) in &points {
            let dist = pos.distance(mp);
            if dist < min_dist {
                min_dist = dist;
                hovered_idx = Some(*i);
            }
        }
    }

    for (i, pos, is_blocked) in points {
        let base_color = if is_blocked {
            SERVER_BLOCKED
        } else {
            SERVER_ACTIVE
        };

        let is_hovered = hovered_idx == Some(i);
        let color = if is_hovered {
            SERVER_HOVERED
        } else {
            base_color
        };

        // Outer glow
        painter.circle_filled(pos, dot_radius + 4.0, color.linear_multiply(0.1));
        painter.circle_filled(pos, dot_radius + 2.0, color.linear_multiply(0.15));
        // Main dot
        painter.circle_filled(pos, dot_radius, color);
        // Inner highlight
        painter.circle_filled(
            Pos2::new(pos.x - 1.0, pos.y - 1.0),
            dot_radius * 0.4,
            Color32::WHITE.linear_multiply(0.2),
        );
    }

    hovered_idx
}

/// Handle map interaction (pan & zoom)
pub fn handle_map_input(ui: &egui::Ui, map_rect: Rect, state: &mut MapState) {
    let response = ui.interact(
        map_rect,
        ui.id().with("map_interact"),
        egui::Sense::click_and_drag(),
    );

    // Zoom with scroll
    if let Some(hover_pos) = ui.input(|i| i.pointer.hover_pos()) {
        if map_rect.contains(hover_pos) {
            let scroll = ui.input(|i| i.smooth_scroll_delta.y);
            if scroll != 0.0 {
                let factor = 1.0 + scroll * 0.003;
                let old_zoom = state.zoom;
                state.zoom = (state.zoom * factor).clamp(1.0, 5.0);

                // Adjust offset so the point under the cursor stays fixed
                if state.zoom != old_zoom {
                    let cursor_rel = hover_pos - map_rect.center();
                    let scale = state.zoom / old_zoom;
                    state.offset = (state.offset + cursor_rel) * scale - cursor_rel;
                }
            }
        }
    }

    // Pan with drag
    if response.dragged() {
        state.offset += ui.input(|i| i.pointer.delta());
    }

    // Clamp offset so the map never pans beyond its edges
    let max_offset_x = map_rect.width() * (state.zoom - 1.0) / 2.0;
    let max_offset_y = map_rect.height() * (state.zoom - 1.0) / 2.0;
    state.offset.x = state.offset.x.clamp(-max_offset_x, max_offset_x);
    state.offset.y = state.offset.y.clamp(-max_offset_y, max_offset_y);

    // Reset offset when fully zoomed out
    if state.zoom <= 1.0 {
        state.offset = Vec2::ZERO;
    }
}
