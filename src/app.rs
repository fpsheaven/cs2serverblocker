use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::mpsc::{self, Receiver, TryRecvError};

use eframe::egui;
use egui::{Color32, CornerRadius, RichText, Stroke};

use crate::api::{self, ServerLocation};
use crate::firewall;
use crate::map::{self, MapState};
use crate::world_data::Region;

type ServerLoadResult = Result<Vec<ServerLocation>, String>;
type BlockedLoadResult = Result<HashSet<String>, String>;

#[derive(Default, Clone)]
struct CountryStat {
    total: usize,
    blocked: usize,
}

/// Per-country entry: (country_name, total_servers, blocked_servers)
type CountryEntry = (String, usize, usize);

struct CachedCountryPanel {
    version: u64,
    continent_totals: BTreeMap<&'static str, (usize, usize, usize)>,
    /// Unified country list per continent sorted: blocked first (by blocked count desc), then unblocked (alphabetical)
    countries_by_continent: BTreeMap<&'static str, Vec<CountryEntry>>,
    blocked_count: usize,
    unblocked_count: usize,
}

struct CountryActionOutcome {
    country: String,
    should_block: bool,
    changed_codes: Vec<String>,
    /// Codes that were unblocked (used by mixed except operations)
    unblocked_codes: Vec<String>,
    skipped: usize,
    failed: usize,
    first_error: Option<String>,
}

struct CountryActionPending {
    country: String,
    should_block: bool,
}

struct PingVerificationWorkerResult {
    code: String,
    blocked: bool,
    checked_relays: usize,
    reachable_relays: usize,
}

struct PingVerificationOutcome {
    results: Vec<PingVerificationWorkerResult>,
}

struct PingVerificationResult {
    blocked: bool,
    checked_relays: usize,
    reachable_relays: usize,
    _checked_at: f64,
}

struct BlockedSyncOutcome {
    version: u64,
    result: BlockedLoadResult,
}

// Theme colors
const BG_DARK: Color32 = Color32::from_rgb(15, 17, 24);
const PANEL_BG: Color32 = Color32::from_rgb(18, 20, 28);
const BORDER: Color32 = Color32::from_rgb(35, 40, 50);
const TEXT_DIM: Color32 = Color32::from_rgb(100, 105, 120);
const GREEN: Color32 = Color32::from_rgb(60, 200, 60);
const RED: Color32 = Color32::from_rgb(220, 60, 60);
const YELLOW: Color32 = Color32::from_rgb(220, 180, 50);
const CONTINENT_ORDER: [&str; 7] = [
    "North America",
    "South America",
    "Europe",
    "Asia",
    "Africa",
    "Oceania",
    "Unknown",
];

pub struct App {
    servers: Vec<ServerLocation>,
    /// Pre-computed country name per server (same indices as `servers`).
    server_countries: Vec<String>,
    blocked: HashSet<String>,
    map_state: MapState,
    loading: bool,
    error: Option<String>,
    is_admin: bool,
    status_message: Option<(String, f64)>,
    search_query: String,
    search_results: Vec<usize>,
    load_rx: Option<Receiver<ServerLoadResult>>,
    blocked_sync_rx: Option<Receiver<BlockedSyncOutcome>>,
    blocked_sync_in_progress: bool,
    blocked_state_version: u64,
    country_action_rx: Option<Receiver<CountryActionOutcome>>,
    country_action_pending: Option<CountryActionPending>,
    ping_results: HashMap<String, PingVerificationResult>,
    ping_rx: Option<Receiver<PingVerificationOutcome>>,
    cached_panel: Option<CachedCountryPanel>,
    cached_region_stats: Option<(u64, HashMap<Region, (usize, usize)>)>,
    selected_countries: HashSet<String>,
    except_selections: HashMap<String, usize>,
    /// Countries whose individual server list is expanded in the side panel.
    expanded_countries: HashSet<String>,
}

impl App {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        apply_theme(&cc.egui_ctx);
        let is_admin = firewall::is_admin();

        let mut app = Self {
            servers: Vec::new(),
            server_countries: Vec::new(),
            blocked: HashSet::new(),
            map_state: MapState::default(),
            loading: false,
            error: None,
            is_admin,
            status_message: None,
            search_query: String::new(),
            search_results: Vec::new(),
            load_rx: None,
            blocked_sync_rx: None,
            blocked_sync_in_progress: false,
            blocked_state_version: 0,
            country_action_rx: None,
            country_action_pending: None,
            ping_results: HashMap::new(),
            ping_rx: None,
            cached_panel: None,
            cached_region_stats: None,
            selected_countries: HashSet::new(),
            except_selections: HashMap::new(),
            expanded_countries: HashSet::new(),
        };

        app.start_loading();

        app
    }

    fn start_loading(&mut self) {
        self.loading = true;
        self.error = None;
        self.ping_results.clear();
        self.blocked.clear();
        self.blocked_state_version = 0;
        self.blocked_sync_in_progress = false;
        self.blocked_sync_rx = None;

        let (tx, rx) = mpsc::channel::<ServerLoadResult>();

        std::thread::spawn(move || {
            let result = api::fetch_server_locations();
            let _ = tx.send(result);
        });

        self.load_rx = Some(rx);
    }

    fn poll_loading(&mut self, ctx: &egui::Context) {
        let recv_result = match self.load_rx.as_ref() {
            Some(rx) => rx.try_recv(),
            None => return,
        };

        match recv_result {
            Ok(Ok(servers)) => {
                self.server_countries = servers.iter().map(|s| infer_country(&s.desc)).collect();
                self.servers = servers;
                self.blocked.clear();
                self.error = None;
                self.loading = false;
                self.load_rx = None;
                self.ping_results.clear();
                self.update_search();
                self.set_status(format!("Loaded {} servers", self.servers.len()), ctx);

                if self.is_admin {
                    self.start_blocked_sync(ctx);
                }
            }
            Ok(Err(err)) => {
                self.loading = false;
                self.error = Some(err);
                self.load_rx = None;
                self.servers.clear();
                self.blocked.clear();
                self.search_results.clear();
            }
            Err(TryRecvError::Empty) => {
                ctx.request_repaint();
            }
            Err(TryRecvError::Disconnected) => {
                self.loading = false;
                self.error = Some("Background loader stopped unexpectedly".to_string());
                self.load_rx = None;
            }
        }
    }

    fn start_blocked_sync(&mut self, ctx: &egui::Context) {
        if !self.is_admin || self.blocked_sync_in_progress {
            return;
        }

        let version = self.blocked_state_version;
        let (tx, rx) = mpsc::channel::<BlockedSyncOutcome>();

        std::thread::spawn(move || {
            let blocked = firewall::get_blocked_servers();
            let _ = tx.send(BlockedSyncOutcome {
                version,
                result: Ok(blocked),
            });
        });

        self.blocked_sync_rx = Some(rx);
        self.blocked_sync_in_progress = true;
        self.set_status("Loaded servers. Syncing firewall rule status...", ctx);
    }

    fn poll_blocked_sync(&mut self, ctx: &egui::Context) {
        let recv_result = match self.blocked_sync_rx.as_ref() {
            Some(rx) => rx.try_recv(),
            None => return,
        };

        match recv_result {
            Ok(outcome) => {
                self.blocked_sync_in_progress = false;
                self.blocked_sync_rx = None;

                if outcome.version != self.blocked_state_version {
                    self.set_status("Skipped stale firewall sync result", ctx);
                    return;
                }

                match outcome.result {
                    Ok(blocked) => {
                        self.blocked = blocked;
                        self.blocked_state_version = self.blocked_state_version.wrapping_add(1);
                        self.set_status(
                            format!("Firewall sync complete: {} blocked", self.blocked.len()),
                            ctx,
                        );
                    }
                    Err(err) => {
                        self.set_status(format!("Firewall sync failed: {}", err), ctx);
                    }
                }
            }
            Err(TryRecvError::Empty) => {
                ctx.request_repaint();
            }
            Err(TryRecvError::Disconnected) => {
                self.blocked_sync_in_progress = false;
                self.blocked_sync_rx = None;
                self.set_status("Firewall sync worker stopped unexpectedly", ctx);
            }
        }
    }

    fn poll_country_action(&mut self, ctx: &egui::Context) {
        let recv_result = match self.country_action_rx.as_ref() {
            Some(rx) => rx.try_recv(),
            None => return,
        };

        match recv_result {
            Ok(outcome) => {
                let changed_count = outcome.changed_codes.len() + outcome.unblocked_codes.len();
                for code in outcome.changed_codes {
                    self.ping_results.remove(&code);
                    if outcome.should_block {
                        self.blocked.insert(code);
                    } else {
                        self.blocked.remove(&code);
                    }
                }
                for code in outcome.unblocked_codes {
                    self.ping_results.remove(&code);
                    self.blocked.remove(&code);
                }
                if changed_count > 0 {
                    self.blocked_state_version = self.blocked_state_version.wrapping_add(1);
                }

                let action = if outcome.should_block {
                    "Blocked"
                } else {
                    "Unblocked"
                };
                if outcome.failed > 0 {
                    let reason = outcome
                        .first_error
                        .unwrap_or_else(|| "unknown error".to_string());
                    self.set_status(
                        format!(
                            "{} {}: {} changed, {} skipped, {} failed ({})",
                            action,
                            outcome.country,
                            changed_count,
                            outcome.skipped,
                            outcome.failed,
                            reason
                        ),
                        ctx,
                    );
                } else {
                    self.set_status(
                        format!(
                            "{} {} complete: {} changed, {} skipped",
                            action, outcome.country, changed_count, outcome.skipped
                        ),
                        ctx,
                    );
                }
                self.country_action_rx = None;
                self.country_action_pending = None;
            }
            Err(TryRecvError::Empty) => {
                ctx.request_repaint();
            }
            Err(TryRecvError::Disconnected) => {
                self.country_action_rx = None;
                self.country_action_pending = None;
                self.set_status("Country action worker stopped unexpectedly", ctx);
            }
        }
    }

    fn poll_ping_verification(&mut self, ctx: &egui::Context) {
        let recv_result = match self.ping_rx.as_ref() {
            Some(rx) => rx.try_recv(),
            None => return,
        };

        match recv_result {
            Ok(outcome) => {
                let checked_at = ctx.input(|i| i.time);
                let count = outcome.results.len();
                for result in outcome.results {
                    self.ping_results.insert(
                        result.code.clone(),
                        PingVerificationResult {
                            blocked: result.blocked,
                            checked_relays: result.checked_relays,
                            reachable_relays: result.reachable_relays,
                            _checked_at: checked_at,
                        },
                    );
                }
                self.ping_rx = None;
                self.set_status(
                    format!("Ping verification complete for {} server(s)", count),
                    ctx,
                );
            }
            Err(TryRecvError::Empty) => {
                ctx.request_repaint();
            }
            Err(TryRecvError::Disconnected) => {
                self.ping_rx = None;
                self.set_status("Ping verification worker stopped unexpectedly", ctx);
            }
        }
    }

    fn start_ping_verification(&mut self, indices: Vec<usize>, ctx: &egui::Context) {
        if self.ping_rx.is_some() {
            self.set_status("Ping verification already running", ctx);
            return;
        }

        let mut targets = Vec::new();
        for idx in indices {
            if idx >= self.servers.len() {
                continue;
            }
            let server = &self.servers[idx];
            let relay_ips = server
                .relays
                .iter()
                .map(|relay| relay.ipv4.clone())
                .collect::<Vec<_>>();
            targets.push((
                server.code.clone(),
                self.blocked.contains(&server.code),
                relay_ips,
            ));
        }

        if targets.is_empty() {
            self.set_status("No servers selected for ping verification", ctx);
            return;
        }

        self.set_status(
            format!(
                "Running ping verification for {} server(s)...",
                targets.len()
            ),
            ctx,
        );

        let (tx, rx) = mpsc::channel::<PingVerificationOutcome>();
        self.ping_rx = Some(rx);

        std::thread::spawn(move || {
            // Ping all servers in parallel using scoped threads.
            let results = std::thread::scope(|s| {
                let handles: Vec<_> = targets
                    .into_iter()
                    .map(|(code, blocked, relay_ips)| {
                        s.spawn(move || {
                            let checked_relays = relay_ips.len().min(3);
                            let mut reachable_relays = 0usize;

                            for ip in relay_ips.iter().take(checked_relays) {
                                if ping_host_once(ip) {
                                    reachable_relays += 1;
                                }
                            }

                            PingVerificationWorkerResult {
                                code,
                                blocked,
                                checked_relays,
                                reachable_relays,
                            }
                        })
                    })
                    .collect();

                handles.into_iter().map(|h| h.join().unwrap()).collect()
            });

            let _ = tx.send(PingVerificationOutcome { results });
        });
    }

    fn ping_status_for_server(&self, code: &str) -> Option<(String, Color32)> {
        let result = self.ping_results.get(code)?;

        if result.blocked {
            Some(("Server Unreachable. Block works".to_string(), GREEN))
        } else if result.reachable_relays > 0 {
            Some((
                format!(
                    "Reachable {}/{}",
                    result.reachable_relays, result.checked_relays
                ),
                GREEN,
            ))
        } else {
            Some((
                format!(
                    "Unreachable 0/{} (may block ICMP)",
                    result.checked_relays
                ),
                YELLOW,
            ))
        }
    }

    fn server_indices_for_country(&self, country: &str) -> Vec<usize> {
        self.server_countries
            .iter()
            .enumerate()
            .filter_map(|(idx, c)| if c == country { Some(idx) } else { None })
            .collect()
    }

    fn country_ping_summary(&self, country: &str) -> Option<(String, Color32)> {
        let mut ok = 0usize;
        let mut warn = 0usize;
        let mut any_blocked = false;

        for (idx, server) in self.servers.iter().enumerate() {
            if self.server_countries[idx] != country {
                continue;
            }
            let Some(result) = self.ping_results.get(&server.code) else {
                continue;
            };

            if result.blocked {
                any_blocked = true;
                ok += 1;
            } else if result.reachable_relays > 0 {
                ok += 1;
            } else {
                warn += 1;
            }
        }

        let tested = ok + warn;
        if tested == 0 {
            return None;
        }

        if warn > 0 {
            Some((
                format!("{}/{} ok, {}/{} unreachable", ok, tested, warn, tested),
                YELLOW,
            ))
        } else if any_blocked {
            Some(("Server Unreachable. Block works".to_string(), GREEN))
        } else {
            Some((format!("{}/{} reachable", ok, tested), GREEN))
        }
    }

    fn show_ping_running_indicator(&self, ui: &mut egui::Ui) {
        if self.ping_rx.is_some() {
            ui.horizontal(|ui| {
                ui.spinner();
                ui.label(
                    RichText::new("Ping verification running...")
                        .color(YELLOW)
                        .small(),
                );
            });
        }
    }

    fn render_country_row(
        &self,
        ui: &mut egui::Ui,
        country: &str,
        row_text: String,
        row_color: Color32,
        should_block: bool,
        country_action: &mut Option<(String, bool)>,
        country_ping_action: &mut Option<String>,
        selected: &mut HashSet<String>,
        ping_summary: Option<(String, Color32)>,
        ping_enabled: bool,
    ) {
        ui.horizontal(|ui| {
            let mut is_selected = selected.contains(country);
            let before = is_selected;
            ui.checkbox(&mut is_selected, "");
            if is_selected != before {
                if is_selected {
                    selected.insert(country.to_string());
                } else {
                    selected.remove(country);
                }
            }

            if ui.add(
                egui::Label::new(RichText::new(row_text).color(row_color).small())
                    .sense(egui::Sense::click()),
            ).clicked() {
                *country_action = Some((country.to_string(), should_block));
            }

            if ui
                .add_enabled(
                    ping_enabled,
                    egui::Button::new(RichText::new("Ping").small()),
                )
                .clicked()
            {
                *country_ping_action = Some(country.to_string());
            }

            if let Some((summary_text, summary_color)) = ping_summary.as_ref() {
                ui.label(
                    RichText::new(summary_text)
                        .color(*summary_color)
                        .small()
                        .monospace(),
                );
            }
        });
    }

    fn set_status(&mut self, msg: impl Into<String>, ctx: &egui::Context) {
        self.status_message = Some((msg.into(), ctx.input(|i| i.time)));
    }

    fn toggle_server(&mut self, idx: usize, ctx: &egui::Context) {
        let server = &self.servers[idx];
        let was_blocked = self.blocked.contains(&server.code);
        let scope_name = server.desc.clone();
        let target = vec![server.clone()];
        self.start_block_state_for_servers(scope_name, !was_blocked, target, ctx);
    }

    fn update_search(&mut self) {
        let q = self.search_query.to_lowercase();
        if q.is_empty() {
            self.search_results.clear();
            return;
        }
        self.search_results = self
            .servers
            .iter()
            .enumerate()
            .filter(|(_, s)| {
                s.desc.to_lowercase().contains(&q) || s.code.to_lowercase().contains(&q)
            })
            .map(|(i, _)| i)
            .collect();
    }

    fn ensure_cached_panel(&mut self) {
        if let Some(ref cached) = self.cached_panel {
            if cached.version == self.blocked_state_version {
                return;
            }
        }

        let mut country_stats: BTreeMap<String, CountryStat> = BTreeMap::new();
        for (idx, server) in self.servers.iter().enumerate() {
            let country = self.server_countries[idx].clone();
            let entry = country_stats.entry(country).or_default();
            entry.total += 1;
            if self.blocked.contains(&server.code) {
                entry.blocked += 1;
            }
        }

        let mut continent_totals: BTreeMap<&'static str, (usize, usize, usize)> = BTreeMap::new();
        let mut countries_by_continent: BTreeMap<&'static str, Vec<CountryEntry>> = BTreeMap::new();
        let mut blocked_count = 0usize;
        let mut unblocked_count = 0usize;

        for (country, stat) in &country_stats {
            let continent = continent_for_country(country);
            let summary = continent_totals.entry(continent).or_insert((0, 0, 0));
            summary.0 += stat.total;
            summary.1 += stat.blocked;
            summary.2 += 1;

            if stat.blocked > 0 {
                blocked_count += 1;
            } else {
                unblocked_count += 1;
            }

            countries_by_continent.entry(continent).or_default().push((
                country.clone(),
                stat.total,
                stat.blocked,
            ));
        }

        // Sort: blocked countries first (by blocked desc), then unblocked (alphabetical)
        for countries in countries_by_continent.values_mut() {
            countries.sort_by(|a, b| {
                let a_blocked = a.2 > 0;
                let b_blocked = b.2 > 0;
                b_blocked.cmp(&a_blocked)
                    .then_with(|| if a_blocked && b_blocked { b.2.cmp(&a.2).then_with(|| a.0.cmp(&b.0)) } else { a.0.cmp(&b.0) })
            });
        }

        self.cached_panel = Some(CachedCountryPanel {
            version: self.blocked_state_version,
            continent_totals,
            countries_by_continent,
            blocked_count,
            unblocked_count,
        });
    }

    fn region_stats(&mut self) -> HashMap<Region, (usize, usize)> {
        if let Some((version, ref stats)) = self.cached_region_stats {
            if version == self.blocked_state_version {
                return stats.clone();
            }
        }
        let stats = map::compute_region_stats(&self.servers, &self.blocked);
        self.cached_region_stats = Some((self.blocked_state_version, stats.clone()));
        stats
    }

    fn start_block_state_for_servers(
        &mut self,
        scope_name: String,
        should_block: bool,
        target_servers: Vec<ServerLocation>,
        ctx: &egui::Context,
    ) {
        if self.country_action_rx.is_some() {
            self.set_status("A block/unblock operation is already running", ctx);
            return;
        }

        if !self.is_admin {
            self.set_status("Run as Administrator to manage firewall rules!", ctx);
            return;
        }

        if target_servers.is_empty() {
            self.set_status(format!("No servers found for {}", scope_name), ctx);
            return;
        }

        // Invalidate any in-flight blocked-state sync while we mutate firewall rules.
        self.blocked_state_version = self.blocked_state_version.wrapping_add(1);

        let blocked_snapshot = self.blocked.clone();
        let action_name = if should_block {
            "Blocking"
        } else {
            "Unblocking"
        };
        self.set_status(
            format!(
                "{} {} ({} servers)...",
                action_name,
                scope_name,
                target_servers.len()
            ),
            ctx,
        );

        let (tx, rx) = mpsc::channel::<CountryActionOutcome>();
        self.country_action_rx = Some(rx);
        self.country_action_pending = Some(CountryActionPending {
            country: scope_name.clone(),
            should_block,
        });

        std::thread::spawn(move || {
            // Filter to only servers that actually need the action
            let servers_to_process: Vec<_> = target_servers
                .into_iter()
                .filter(|s| {
                    let currently_blocked = blocked_snapshot.contains(&s.code);
                    if should_block { !currently_blocked } else { currently_blocked }
                })
                .collect();

            let skipped = 0usize; // skipped servers were filtered out above
            let total = servers_to_process.len();

            // Single batched firewall call for ALL servers at once
            let result = if should_block {
                firewall::block_servers(&servers_to_process)
            } else {
                firewall::unblock_servers(&servers_to_process)
            };

            let (changed_codes, failed, first_error) = match result {
                Ok(()) => {
                    let codes = servers_to_process.iter().map(|s| s.code.clone()).collect();
                    (codes, 0usize, None)
                }
                Err(err) => (Vec::new(), total, Some(err)),
            };

            let _ = tx.send(CountryActionOutcome {
                country: scope_name,
                should_block,
                changed_codes,
                unblocked_codes: Vec::new(),
                skipped,
                failed,
                first_error,
            });
        });
    }

    /// Block some servers and unblock others in a single background operation.
    fn start_except_block(
        &mut self,
        scope_name: String,
        to_block: Vec<ServerLocation>,
        to_unblock: Vec<ServerLocation>,
        ctx: &egui::Context,
    ) {
        if self.country_action_rx.is_some() {
            self.set_status("A block/unblock operation is already running", ctx);
            return;
        }

        if !self.is_admin {
            self.set_status("Run as Administrator to manage firewall rules!", ctx);
            return;
        }

        if to_block.is_empty() && to_unblock.is_empty() {
            self.set_status(format!("No changes needed for {}", scope_name), ctx);
            return;
        }

        self.blocked_state_version = self.blocked_state_version.wrapping_add(1);

        let total = to_block.len() + to_unblock.len();
        self.set_status(
            format!("Applying {} ({} servers)...", scope_name, total),
            ctx,
        );

        let (tx, rx) = mpsc::channel::<CountryActionOutcome>();
        self.country_action_rx = Some(rx);
        self.country_action_pending = Some(CountryActionPending {
            country: scope_name.clone(),
            should_block: true,
        });

        let blocked_snapshot = self.blocked.clone();

        std::thread::spawn(move || {
            let mut blocked_codes = Vec::new();
            let mut unblocked_codes = Vec::new();
            let mut failed = 0usize;
            let mut first_error: Option<String> = None;

            // Block non-excepted servers
            let servers_to_block: Vec<_> = to_block
                .into_iter()
                .filter(|s| !blocked_snapshot.contains(&s.code))
                .collect();
            if !servers_to_block.is_empty() {
                match firewall::block_servers(&servers_to_block) {
                    Ok(()) => {
                        blocked_codes.extend(servers_to_block.iter().map(|s| s.code.clone()));
                    }
                    Err(e) => {
                        failed += servers_to_block.len();
                        if first_error.is_none() {
                            first_error = Some(e);
                        }
                    }
                }
            }

            // Unblock excepted servers
            let servers_to_unblock: Vec<_> = to_unblock
                .into_iter()
                .filter(|s| blocked_snapshot.contains(&s.code))
                .collect();
            if !servers_to_unblock.is_empty() {
                match firewall::unblock_servers(&servers_to_unblock) {
                    Ok(()) => {
                        unblocked_codes.extend(servers_to_unblock.iter().map(|s| s.code.clone()));
                    }
                    Err(e) => {
                        failed += servers_to_unblock.len();
                        if first_error.is_none() {
                            first_error = Some(e);
                        }
                    }
                }
            }

            let _ = tx.send(CountryActionOutcome {
                country: scope_name,
                should_block: true,
                changed_codes: blocked_codes,
                unblocked_codes,
                skipped: 0,
                failed,
                first_error,
            });
        });
    }

    fn start_country_block_state(
        &mut self,
        country: &str,
        should_block: bool,
        ctx: &egui::Context,
    ) {
        let country_owned = country.to_string();
        let target_servers = self
            .servers
            .iter()
            .enumerate()
            .filter(|(idx, _)| self.server_countries[*idx] == country_owned)
            .map(|(_, s)| s.clone())
            .collect::<Vec<_>>();

        self.start_block_state_for_servers(country_owned, should_block, target_servers, ctx);
    }

    fn start_continent_block_state(
        &mut self,
        continent: &str,
        should_block: bool,
        ctx: &egui::Context,
    ) {
        let continent_owned = continent.to_string();
        let target_servers = self
            .servers
            .iter()
            .enumerate()
            .filter(|(idx, _)| {
                continent_for_country(&self.server_countries[*idx]) == continent_owned
            })
            .map(|(_, s)| s.clone())
            .collect::<Vec<_>>();

        self.start_block_state_for_servers(continent_owned, should_block, target_servers, ctx);
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.poll_loading(ctx);
        self.poll_blocked_sync(ctx);
        self.poll_country_action(ctx);
        self.poll_ping_verification(ctx);

        let mut toggle_action: Option<usize> = None;
        let mut country_action: Option<(String, bool)> = None;
        let continent_action: Option<(String, bool)> = None;
        let mut country_ping_action: Option<String> = None;
        let mut mass_block_action: Option<bool> = None;
        let mut unblock_all_action = false;
        let mut except_block_action: Option<(String, String)> = None;
        let mut server_toggle_action: Option<(String, bool)> = None; // (server_code, should_block)

        // Admin warning (if not admin, show as a small top bar instead of bottom status bar)
        if !self.is_admin {
            egui::TopBottomPanel::bottom("admin_bar")
                .frame(
                    egui::Frame::new()
                        .fill(PANEL_BG)
                        .stroke(Stroke::new(1.0, BORDER))
                        .inner_margin(egui::Margin::symmetric(12, 4)),
                )
                .show(ctx, |ui| {
                    ui.horizontal(|ui| {
                        if ui
                            .small_button(RichText::new("NOT ADMIN - Click to elevate").color(YELLOW))
                            .clicked()
                        {
                            if let Err(err) = firewall::relaunch_as_admin() {
                                self.set_status(format!("Elevation failed: {}", err), ctx);
                            }
                        }
                    });
                });
        }

        // Dedicated country list panel (separate from globe UI).
        egui::SidePanel::right("countries_panel")
            .resizable(true)
            .default_width(420.0)
            .min_width(320.0)
            .frame(
                egui::Frame::new()
                    .fill(PANEL_BG)
                    .stroke(Stroke::new(1.0, BORDER))
                    .inner_margin(egui::Margin::same(10)),
            )
            .show(ctx, |ui| {
                let mut panel_style = ui.style().as_ref().clone();
                panel_style
                    .text_styles
                    .insert(egui::TextStyle::Small, egui::FontId::proportional(14.0));
                panel_style
                    .text_styles
                    .insert(egui::TextStyle::Body, egui::FontId::proportional(15.0));
                panel_style
                    .text_styles
                    .insert(egui::TextStyle::Button, egui::FontId::proportional(15.0));
                ui.set_style(panel_style);

                ui.label(
                    RichText::new("Countries")
                        .color(Color32::WHITE)
                        .strong()
                        .small(),
                );
                ui.label(
                    RichText::new(
                        "Click a country row to toggle one country. Click a continent header to toggle that continent.",
                    )
                    .color(TEXT_DIM)
                    .small(),
                );
                ui.separator();

                ui.label(
                    RichText::new("Search Servers")
                        .color(Color32::WHITE)
                        .small(),
                );
                let search_response = ui.text_edit_singleline(&mut self.search_query);
                if search_response.changed() {
                    self.update_search();
                }

                if !self.search_query.is_empty() {
                    ui.add_space(4.0);
                    ui.label(
                        RichText::new(format!("{} matches", self.search_results.len()))
                            .color(TEXT_DIM)
                            .small(),
                    );

                    egui::ScrollArea::vertical()
                        .id_salt("search_side_scroll")
                        .max_height(150.0)
                        .show(ui, |ui| {
                            if self.search_results.is_empty() {
                                ui.label(RichText::new("No results").color(TEXT_DIM).small());
                            } else {
                                for &idx in self.search_results.iter().take(12) {
                                    let server = &self.servers[idx];
                                    let is_blocked = self.blocked.contains(&server.code);
                                    let status_color = if is_blocked { RED } else { GREEN };
                                    let status_icon = if is_blocked { "X" } else { "O" };

                                    ui.horizontal(|ui| {
                                        if ui
                                            .small_button(
                                                RichText::new(status_icon)
                                                    .color(status_color)
                                                    .small()
                                                    .monospace(),
                                            )
                                            .clicked()
                                        {
                                            toggle_action = Some(idx);
                                        }

                                        if ui
                                            .selectable_label(
                                                false,
                                                RichText::new(format!(
                                                    "{} [{}]",
                                                    server.desc, server.code
                                                ))
                                                .color(if is_blocked {
                                                    Color32::from_rgb(180, 80, 80)
                                                } else {
                                                    Color32::from_rgb(180, 180, 200)
                                                })
                                                .small(),
                                            )
                                            .clicked()
                                        {
                                            toggle_action = Some(idx);
                                        }

                                        if let Some((ping_text, ping_color)) =
                                            self.ping_status_for_server(&server.code)
                                        {
                                            ui.label(
                                                RichText::new(ping_text)
                                                    .color(ping_color)
                                                    .small()
                                                    .monospace(),
                                            );
                                        }
                                    });
                                }

                                if self.search_results.len() > 12 {
                                    ui.label(
                                        RichText::new(format!(
                                            "... and {} more",
                                            self.search_results.len() - 12
                                        ))
                                        .color(TEXT_DIM)
                                        .small(),
                                    );
                                }
                            }
                        });

                    let listed_indices =
                        self.search_results.iter().copied().take(12).collect::<Vec<_>>();

                    ui.add_space(6.0);
                    ui.separator();
                    ui.add_space(4.0);
                    ui.label(
                        RichText::new("Rule Verification (Ping)")
                            .color(Color32::WHITE)
                            .small(),
                    );
                    ui.label(
                        RichText::new(
                            "Verifies listed servers with ping. PASS/FAIL is based on expected blocked state.",
                        )
                        .color(TEXT_DIM)
                        .small(),
                    );
                    ui.label(
                        RichText::new("Note: some servers may block ICMP ping.")
                            .color(TEXT_DIM)
                            .small(),
                    );

                    let mut verify_ping = false;
                    let mut clear_ping = false;
                    ui.horizontal(|ui| {
                        if ui
                            .add_enabled(
                                self.ping_rx.is_none(),
                                egui::Button::new("Verify Listed by Ping"),
                            )
                            .clicked()
                        {
                            verify_ping = true;
                        }

                        if ui
                            .add_enabled(
                                self.ping_rx.is_none(),
                                egui::Button::new("Clear Ping Results"),
                            )
                            .clicked()
                        {
                            clear_ping = true;
                        }
                    });
                    if verify_ping {
                        self.start_ping_verification(listed_indices, ctx);
                    } else if clear_ping {
                        for idx in listed_indices {
                            if let Some(server) = self.servers.get(idx) {
                                self.ping_results.remove(&server.code);
                            }
                        }
                    }

                    self.show_ping_running_indicator(ui);
                }

                ui.add_space(8.0);
                ui.separator();

                if self.loading {
                    return;
                }

                if let Some(ref err) = self.error {
                    ui.label(
                        RichText::new("Failed to load country list")
                            .color(RED)
                            .small()
                            .strong(),
                    );
                    ui.label(
                        RichText::new(err)
                            .color(Color32::from_rgb(190, 195, 210))
                            .small(),
                    );
                    if ui.button("Retry").clicked() {
                        self.start_loading();
                    }
                    return;
                }

                self.ensure_cached_panel();
                let panel = self.cached_panel.as_ref().unwrap();
                let blocked_count = panel.blocked_count;
                let unblocked_count = panel.unblocked_count;
                let continent_totals = panel.continent_totals.clone();
                let countries_by_continent = panel.countries_by_continent.clone();

                ui.label(
                    RichText::new(format!(
                        "{} blocked, {} not blocked",
                        blocked_count, unblocked_count
                    ))
                    .color(TEXT_DIM)
                    .small(),
                );
                ui.separator();
                let buttons_enabled = self.country_action_rx.is_none() && !self.blocked_sync_in_progress;
                ui.add_enabled_ui(buttons_enabled, |ui| {
                    ui.horizontal(|ui| {
                        let sel_count = self.selected_countries.len();
                        if ui.button(RichText::new(format!("Block Selected ({})", sel_count)).small()).clicked() && sel_count > 0 {
                            mass_block_action = Some(true);
                        }
                        if ui.button(RichText::new(format!("Unblock Selected ({})", sel_count)).small()).clicked() && sel_count > 0 {
                            mass_block_action = Some(false);
                        }
                        if blocked_count > 0 && ui.button(RichText::new("Unblock All").small()).clicked() {
                            unblock_all_action = true;
                        }
                        if ui.button(RichText::new("Refresh").small()).clicked() {
                            self.start_loading();
                        }
                    });
                });
                ui.add_space(4.0);
                if self.ping_rx.is_some() {
                    self.show_ping_running_indicator(ui);
                    ui.add_space(4.0);
                }

                let action_running = self.country_action_rx.is_some();
                if let Some(pending) = &self.country_action_pending {
                    let action_text = if pending.should_block {
                        "Blocking"
                    } else {
                        "Unblocking"
                    };
                    ui.horizontal(|ui| {
                        ui.spinner();
                        ui.label(
                            RichText::new(format!("{} {}...", action_text, pending.country))
                                .color(YELLOW)
                                .small(),
                        );
                    });
                    ui.add_space(6.0);
                }

                if self.blocked_sync_in_progress {
                    ui.horizontal(|ui| {
                        ui.spinner();
                        ui.label(RichText::new("Loading rules...").color(YELLOW).small());
                    });
                }

                let ping_enabled = self.ping_rx.is_none();
                let mut selected = std::mem::take(&mut self.selected_countries);
                let mut except_sels = std::mem::take(&mut self.except_selections);
                ui.add_enabled_ui(!action_running, |ui| {
                    egui::ScrollArea::vertical()
                        .id_salt("countries_side_scroll")
                        .show(ui, |ui| {
                            for continent in CONTINENT_ORDER {
                                let Some(countries) = countries_by_continent.get(continent) else {
                                    continue;
                                };
                                if countries.is_empty() {
                                    continue;
                                }
                                let (continent_total, continent_blocked, continent_countries) =
                                    continent_totals
                                        .get(continent)
                                        .copied()
                                        .unwrap_or((0, 0, countries.len()));
                                let continent_header = format!(
                                    "{} ({}) [{}/{} blocked]",
                                    continent,
                                    continent_countries,
                                    continent_blocked,
                                    continent_total
                                );

                                let continent_color = if continent_blocked == continent_total && continent_total > 0 {
                                    RED
                                } else if continent_blocked == 0 {
                                    GREEN
                                } else {
                                    Color32::from_rgb(185, 145, 95) // mixed
                                };

                                let country_names: Vec<&String> = countries.iter().map(|(c, _, _)| c).collect();
                                let all_selected = !country_names.is_empty() && country_names.iter().all(|c| selected.contains(c.as_str()));
                                ui.horizontal(|ui| {
                                    let mut sel = all_selected;
                                    let before = sel;
                                    ui.checkbox(&mut sel, "");
                                    if sel != before {
                                        for c in &country_names {
                                            if sel {
                                                selected.insert(c.to_string());
                                            } else {
                                                selected.remove(c.as_str());
                                            }
                                        }
                                    }
                                    ui.label(
                                        RichText::new(continent_header)
                                            .color(continent_color)
                                            .strong(),
                                    );
                                });
                                ui.add_space(2.0);
                                for (country, total, blocked_servers) in countries {
                                    let is_blocked = *blocked_servers > 0;
                                    let all_blocked = *blocked_servers == *total;
                                    let row_color = if all_blocked {
                                        RED
                                    } else if is_blocked {
                                        Color32::from_rgb(215, 150, 90) // partial
                                    } else {
                                        GREEN
                                    };
                                    let has_multiple = *total > 1;
                                    let is_expanded = self.expanded_countries.contains(country);
                                    let row_text = format!("{} ({}/{})", country, blocked_servers, total);
                                    let ps = self.country_ping_summary(country);
                                    let should_block = !all_blocked;
                                    ui.horizontal(|ui| {
                                        ui.add_space(16.0);
                                        if has_multiple {
                                            let arrow = if is_expanded { "v" } else { ">" };
                                            if ui.button(RichText::new(arrow).small()).clicked() {
                                                if is_expanded {
                                                    self.expanded_countries.remove(country);
                                                } else {
                                                    self.expanded_countries.insert(country.to_string());
                                                }
                                            }
                                        }
                                        self.render_country_row(
                                            ui,
                                            country,
                                            row_text,
                                            row_color,
                                            should_block,
                                            &mut country_action,
                                            &mut country_ping_action,
                                            &mut selected,
                                            ps,
                                            ping_enabled,
                                        );
                                    });
                                    // Show individual servers when expanded
                                    if has_multiple && is_expanded {
                                        let country_servers: Vec<_> = self.servers.iter().enumerate()
                                            .filter(|(idx, _)| self.server_countries[*idx] == *country)
                                            .collect();
                                        for (_idx, server) in &country_servers {
                                            let srv_blocked = self.blocked.contains(&server.code);
                                            let srv_color = if srv_blocked { RED } else { GREEN };
                                            let srv_status = if srv_blocked { "blocked" } else { "active" };
                                            let srv_text = format!("{} [{}]", server.desc, srv_status);
                                            let ps = self.ping_status_for_server(&server.code);
                                            ui.horizontal(|ui| {
                                                ui.add_space(32.0);
                                                ui.label(RichText::new(srv_text).color(srv_color).small());
                                                let btn_label = if srv_blocked { "Unblock" } else { "Block" };
                                                if ui.button(RichText::new(btn_label).small()).clicked() {
                                                    server_toggle_action = Some((server.code.clone(), !srv_blocked));
                                                }
                                                if let Some((ping_text, ping_color)) = ps {
                                                    ui.label(RichText::new(ping_text).color(ping_color).small().monospace());
                                                }
                                            });
                                        }
                                    }
                                }
                                // "Block all except" dropdown (hidden when all are already blocked)
                                let all_country_names: Vec<String> = countries.iter().map(|(c, _, _)| c.clone()).collect();
                                if all_country_names.len() > 1 && continent_blocked < continent_total {
                                    ui.add_space(4.0);
                                    ui.horizontal(|ui| {
                                        ui.add_space(16.0);
                                        ui.label(RichText::new("Block all except").color(TEXT_DIM).small());
                                        let sel_idx = except_sels.entry(continent.to_string()).or_insert(0);
                                        if *sel_idx >= all_country_names.len() {
                                            *sel_idx = 0;
                                        }
                                        egui::ComboBox::from_id_salt(format!("except_{}", continent))
                                            .selected_text(RichText::new(&all_country_names[*sel_idx]).small())
                                            .width(100.0)
                                            .show_ui(ui, |ui| {
                                                for (i, c) in all_country_names.iter().enumerate() {
                                                    ui.selectable_value(sel_idx, i, RichText::new(c).small());
                                                }
                                            });
                                        if ui.button(RichText::new("Apply").small()).clicked() {
                                            except_block_action = Some((continent.to_string(), all_country_names[*sel_idx].clone()));
                                        }
                                    });
                                }
                                ui.add_space(8.0);
                                ui.separator();
                                ui.add_space(4.0);
                            }
                        });
                });
                self.selected_countries = selected;
                self.except_selections = except_sels;
            });

        // Full-screen map
        egui::CentralPanel::default()
            .frame(
                egui::Frame::new()
                    .fill(BG_DARK)
                    .inner_margin(egui::Margin::ZERO),
            )
            .show(ctx, |ui| {
                if self.loading {
                    return;
                }

                if let Some(err) = self.error.clone() {
                    ui.centered_and_justified(|ui| {
                        egui::Frame::new()
                            .fill(PANEL_BG)
                            .stroke(Stroke::new(1.0, BORDER))
                            .corner_radius(CornerRadius::same(8))
                            .inner_margin(egui::Margin::same(16))
                            .show(ui, |ui| {
                                ui.set_max_width(540.0);
                                ui.label(
                                    RichText::new("Failed to load CS2 server data")
                                        .color(RED)
                                        .strong(),
                                );
                                ui.add_space(6.0);
                                ui.label(
                                    RichText::new(err)
                                        .color(Color32::from_rgb(190, 195, 210))
                                        .small(),
                                );
                                ui.add_space(10.0);
                                if ui.button("Retry").clicked() {
                                    self.start_loading();
                                }
                            });
                    });
                    return;
                }

                let map_rect = ui.available_rect_before_wrap();
                let painter = ui.painter_at(map_rect);

                map::draw_map(&painter, map_rect, &self.map_state);

                let mouse_pos = ui.input(|i| i.pointer.hover_pos());
                let hovered = map::draw_server_dots(
                    &painter,
                    map_rect,
                    &self.map_state,
                    &self.servers,
                    &self.blocked,
                    mouse_pos,
                );

                let region_stats = self.region_stats();
                map::draw_region_labels(
                    &painter,
                    map_rect,
                    &self.map_state,
                    &region_stats,
                );

                // Tooltip on hover
                if let Some(idx) = hovered {
                    let server = &self.servers[idx];
                    let is_blocked = self.blocked.contains(&server.code);
                    egui::show_tooltip(ctx, ui.layer_id(), egui::Id::new("server_tip"), |ui| {
                        ui.set_max_width(250.0);
                        ui.label(RichText::new(&server.desc).strong().color(Color32::WHITE));
                        ui.label(
                            RichText::new(format!("Code: {}", server.code))
                                .color(TEXT_DIM)
                                .small(),
                        );
                        for relay in &server.relays {
                            ui.label(
                                RichText::new(format!(
                                    "  {} (:{}-{})",
                                    relay.ipv4, relay.port_range[0], relay.port_range[1]
                                ))
                                .color(Color32::from_rgb(90, 95, 110))
                                .small(),
                            );
                        }
                        let (status, color) = if is_blocked {
                            ("BLOCKED", RED)
                        } else {
                            ("ACTIVE", GREEN)
                        };
                        ui.add_space(2.0);
                        ui.label(RichText::new(status).color(color).strong());
                        ui.label(
                            RichText::new("Click to toggle")
                                .color(TEXT_DIM)
                                .small()
                                .italics(),
                        );
                    });
                }

                map::handle_map_input(ui, map_rect, &mut self.map_state);

                let clicked = ui.input(|i| i.pointer.button_clicked(egui::PointerButton::Primary));
                if clicked {
                    if let Some(idx) = hovered {
                        toggle_action = Some(idx);
                    }
                }
            });

        if unblock_all_action {
            let target: Vec<_> = self.servers.iter()
                .filter(|s| self.blocked.contains(&s.code))
                .cloned()
                .collect();
            if !target.is_empty() {
                self.start_block_state_for_servers("All countries".to_string(), false, target, ctx);
            }
        } else if let Some(should_block) = mass_block_action {
            let selected_set = &self.selected_countries;
            let target: Vec<_> = self.servers.iter().enumerate().filter(|(idx, s)| {
                let country = &self.server_countries[*idx];
                if !selected_set.contains(country) { return false; }
                if should_block { !self.blocked.contains(&s.code) }
                else { self.blocked.contains(&s.code) }
            }).map(|(_, s)| s.clone()).collect();
            if !target.is_empty() {
                let label = format!("{} selected countries", selected_set.len());
                self.start_block_state_for_servers(label, should_block, target, ctx);
            }
            self.selected_countries.clear();
        } else if let Some((continent, except_country)) = except_block_action {
            // Build set of countries in this continent (excluding the excepted one)
            let mut countries_to_block: HashSet<String> = HashSet::new();
            if let Some(panel) = &self.cached_panel {
                if let Some(entries) = panel.countries_by_continent.get(continent.as_str()) {
                    for (c, _, _) in entries {
                        if c != &except_country {
                            countries_to_block.insert(c.clone());
                        }
                    }
                }
            }
            // Block all non-excepted unblocked servers
            let to_block: Vec<_> = self.servers.iter().enumerate()
                .filter(|(idx, s)| {
                    let country = &self.server_countries[*idx];
                    countries_to_block.contains(country) && !self.blocked.contains(&s.code)
                })
                .map(|(_, s)| s.clone())
                .collect();
            // Unblock the excepted country's servers if they are blocked
            let to_unblock: Vec<_> = self.servers.iter().enumerate()
                .filter(|(idx, s)| {
                    let country = &self.server_countries[*idx];
                    country == &except_country
                        && continent_for_country(country) == continent
                        && self.blocked.contains(&s.code)
                })
                .map(|(_, s)| s.clone())
                .collect();
            let label = format!("{} except {}", continent, except_country);
            self.start_except_block(label, to_block, to_unblock, ctx);
        } else if let Some((continent, should_block)) = continent_action {
            self.start_continent_block_state(&continent, should_block, ctx);
        } else if let Some((country, should_block)) = country_action {
            self.start_country_block_state(&country, should_block, ctx);
        }

        if let Some(country) = country_ping_action {
            let indices = self.server_indices_for_country(&country);
            self.start_ping_verification(indices, ctx);
        }

        if let Some(idx) = toggle_action {
            if idx < self.servers.len() {
                self.toggle_server(idx, ctx);
            }
        }

        if let Some((code, should_block)) = server_toggle_action {
            if let Some(server) = self.servers.iter().find(|s| s.code == code) {
                let target = vec![server.clone()];
                self.start_block_state_for_servers(server.desc.clone(), should_block, target, ctx);
            }
        }

        // Full-screen loading overlay
        if self.loading {
            let screen_rect = ctx.screen_rect();
            let overlay_layer = egui::LayerId::new(egui::Order::Foreground, egui::Id::new("loading_overlay"));
            let painter = ctx.layer_painter(overlay_layer);

            // Semi-transparent dark background
            painter.rect_filled(screen_rect, 0.0, Color32::from_rgba_unmultiplied(0, 0, 0, 180));

            // Centered area for spinner + text
            egui::Area::new(egui::Id::new("loading_area"))
                .order(egui::Order::Foreground)
                .anchor(egui::Align2::CENTER_CENTER, [0.0, 0.0])
                .show(ctx, |ui| {
                    ui.vertical_centered(|ui| {
                        ui.spinner();
                        ui.add_space(12.0);
                        ui.label(
                            RichText::new("Refreshing IPs...")
                                .color(Color32::WHITE)
                                .size(18.0)
                                .strong(),
                        );
                    });
                });
        }
    }
}

fn ping_host_once(ip: &str) -> bool {
    #[cfg(windows)]
    let args = ["-n", "1", "-w", "700", ip];

    #[cfg(not(windows))]
    let args = ["-c", "1", "-W", "1", ip];

    let mut cmd = std::process::Command::new("ping");
    cmd.args(args);
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        cmd.creation_flags(0x08000000); // CREATE_NO_WINDOW
    }
    cmd.output()
        .map(|out| out.status.success())
        .unwrap_or(false)
}

fn apply_theme(ctx: &egui::Context) {
    let mut style = (*ctx.style()).clone();
    style.visuals.dark_mode = true;
    style.visuals.panel_fill = BG_DARK;
    style.visuals.window_fill = PANEL_BG;
    style.visuals.widgets.noninteractive.bg_fill = PANEL_BG;
    style.visuals.widgets.inactive.bg_fill = Color32::from_rgb(30, 33, 42);
    style.visuals.widgets.hovered.bg_fill = Color32::from_rgb(40, 45, 55);
    style.visuals.widgets.active.bg_fill = Color32::from_rgb(50, 55, 65);
    style.visuals.override_text_color = Some(Color32::from_rgb(200, 205, 215));
    ctx.set_style(style);
}

fn infer_country(desc: &str) -> String {
    let trimmed = desc.trim();
    if trimmed.is_empty() {
        return "Unknown".to_string();
    }

    if let Some(country) = country_from_parentheses(trimmed) {
        return normalize_country_name(&country);
    }

    if let Some(part) = trimmed.split(',').next_back() {
        let p = part.trim();
        if !p.is_empty() {
            return normalize_country_name(p);
        }
    }

    if let Some(part) = trimmed.split('-').next_back() {
        let p = part.trim();
        if !p.is_empty() && p.len() <= 24 {
            return normalize_country_name(p);
        }
    }

    if let Some((left, _)) = trimmed.split_once(':') {
        let p = left.trim();
        if !p.is_empty() {
            return normalize_country_name(p);
        }
    }

    normalize_country_name(trimmed)
}

fn country_from_parentheses(input: &str) -> Option<String> {
    let start = input.rfind('(')?;
    let end = input.rfind(')')?;
    if end <= start + 1 {
        return None;
    }
    Some(input[start + 1..end].trim().to_string())
}

fn normalize_country_name(raw: &str) -> String {
    let cleaned = raw
        .replace('_', " ")
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ");
    let lower = cleaned.to_lowercase();

    match lower.as_str() {
        "us" | "u.s" | "u.s." | "usa" | "united states" | "united states of america" => {
            return "United States".to_string();
        }
        "uk" | "u.k." | "great britain" | "britain" | "england" => {
            return "United Kingdom".to_string();
        }
        "uae" => {
            return "United Arab Emirates".to_string();
        }
        _ => {}
    }

    cleaned
        .split(' ')
        .filter(|p| !p.is_empty())
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first) => {
                    let first = first.to_uppercase().to_string();
                    let rest = chars.as_str().to_lowercase();
                    format!("{}{}", first, rest)
                }
                None => String::new(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn continent_for_country(country: &str) -> &'static str {
    let lower = country.trim().to_lowercase();

    match lower.as_str() {
        "argentina" | "brazil" | "chile" | "peru" | "colombia" | "ecuador" | "uruguay" => {
            return "South America";
        }
        "australia" | "new zealand" => {
            return "Oceania";
        }
        "south africa" | "morocco" | "egypt" | "kenya" | "nigeria" => {
            return "Africa";
        }
        "austria" | "belgium" | "czech republic" | "denmark" | "finland" | "france" | "germany"
        | "greece" | "hungary" | "ireland" | "italy" | "netherlands" | "norway" | "poland"
        | "portugal" | "romania" | "spain" | "sweden" | "switzerland" | "ukraine"
        | "united kingdom" | "turkey" => {
            return "Europe";
        }
        "china"
        | "hong kong"
        | "india"
        | "japan"
        | "singapore"
        | "south korea"
        | "united arab emirates"
        | "bahrain"
        | "qatar"
        | "taiwan"
        | "malaysia"
        | "thailand"
        | "vietnam"
        | "indonesia"
        | "philippines" => {
            return "Asia";
        }
        "united states" | "canada" | "mexico" | "california" | "illinois" | "texas"
        | "virginia" | "washington" | "oregon" | "florida" | "new york" | "georgia" => {
            return "North America";
        }
        _ => {}
    }

    if lower.contains("hong kong")
        || lower.contains("china")
        || lower.contains("shanghai")
        || lower.contains("japan")
        || lower.contains("singapore")
        || lower.contains("korea")
        || lower.contains("india")
    {
        return "Asia";
    }

    if lower.contains("united states")
        || lower.contains("canada")
        || lower.contains("mexico")
        || lower.contains("california")
        || lower.contains("illinois")
    {
        return "North America";
    }

    "Unknown"
}
