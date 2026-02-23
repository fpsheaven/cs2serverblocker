/// World land mass data as a 72x36 grid (5° per cell).
/// col = (lon + 180) / 5, row = (90 - lat) / 5
/// Land defined as column ranges per row.

pub const GRID_COLS: usize = 72;
pub const GRID_ROWS: usize = 36;

pub type LandGrid = [[bool; GRID_COLS]; GRID_ROWS];

pub fn generate_land_grid() -> LandGrid {
    // Each row: list of (start_col_inclusive, end_col_inclusive) ranges
    let ranges: [&[(usize, usize)]; GRID_ROWS] = [
        // Row 0  (90-85°N)
        &[],
        // Row 1  (85-80°N): Arctic Canada islands, northern Greenland
        &[(14, 18), (24, 30)],
        // Row 2  (80-75°N): Canadian Arctic, Greenland, northern Siberian coast
        &[(10, 20), (22, 32), (52, 66)],
        // Row 3  (75-70°N): Alaska, Canada, Greenland, Svalbard, Russia
        &[(2, 4), (9, 22), (23, 32), (37, 38), (42, 68)],
        // Row 4  (70-65°N): Alaska, Canada, Greenland, Iceland, Scandinavia, Russia
        &[(1, 5), (8, 24), (25, 30), (31, 33), (37, 41), (43, 70)],
        // Row 5  (65-60°N): Alaska, Canada, S. Greenland, UK north, Scandinavia, Russia
        &[(1, 6), (8, 24), (35, 36), (37, 42), (44, 71)],
        // Row 6  (60-55°N): S. Alaska, Canada, UK, Europe, Russia/Siberia
        &[(2, 6), (9, 24), (34, 36), (37, 42), (43, 71)],
        // Row 7  (55-50°N): Kamchatka tip, Canada, UK+France, Eur, Russia
        &[(10, 23), (34, 42), (43, 71)],
        // Row 8  (50-45°N): S. Canada + N. USA, W+C Europe, Russia/Central Asia
        &[(12, 23), (34, 42), (43, 66)],
        // Row 9  (45-40°N): USA, S. Europe, Central Asia / China
        &[(13, 22), (34, 41), (42, 65)],
        // Row 10 (40-35°N): USA, Iberia-Med-Turkey, Iran to China, Japan
        &[(13, 21), (34, 40), (41, 62), (63, 65)],
        // Row 11 (35-30°N): S. USA, N. Africa coast, Middle East, India north, China, Japan
        &[(14, 20), (34, 38), (40, 62), (63, 65)],
        // Row 12 (30-25°N): Mexico, Sahara + Arabia, India, S. China
        &[(15, 19), (33, 50), (51, 56), (57, 63)],
        // Row 13 (25-20°N): Mexico + Caribbean, Sahara, Arabia, India, SE Asia
        &[(15, 18), (32, 49), (50, 56), (57, 61)],
        // Row 14 (20-15°N): C. America, W. Africa + Sahel, India, Indochina, Philippines
        &[(15, 17), (32, 47), (50, 55), (56, 60)],
        // Row 15 (15-10°N): C. America tip, W+C Africa, India, SE Asia
        &[(15, 16), (32, 46), (51, 54), (57, 59)],
        // Row 16 (10-5°N): N. South America, Guinea coast, Indochina/Malaysia
        &[(16, 17), (32, 45), (57, 59)],
        // Row 17 (5°N-0°): N. Brazil/Guyana, Congo, Malaysia, Borneo, Sulawesi-Papua
        &[(17, 21), (33, 44), (55, 58), (60, 63)],
        // Row 18 (0-5°S): Brazil, E. Africa, Sumatra, Borneo+Papua
        &[(17, 24), (35, 44), (55, 57), (59, 64)],
        // Row 19 (5-10°S): Brazil wide, E. Africa, Java, Papua
        &[(18, 27), (37, 44), (55, 56), (60, 65)],
        // Row 20 (10-15°S): Brazil widest, E. Africa, Madagascar, N. Australia
        &[(18, 28), (38, 44), (45, 46), (61, 66)],
        // Row 21 (15-20°S): Brazil, SE Africa, Madagascar, Australia
        &[(18, 27), (39, 44), (45, 46), (59, 67)],
        // Row 22 (20-25°S): Brazil, S. Africa, Madagascar, Australia
        &[(18, 26), (39, 43), (45, 46), (59, 68)],
        // Row 23 (25-30°S): S. Brazil + Paraguay, S. Africa, Australia
        &[(19, 25), (39, 43), (59, 68)],
        // Row 24 (30-35°S): Argentina/Chile, S. Africa tip, Australia
        &[(19, 24), (39, 42), (60, 68)],
        // Row 25 (35-40°S): Argentina, SE Australia, New Zealand
        &[(20, 23), (62, 67), (69, 71)],
        // Row 26 (40-45°S): Patagonia, Tasmania, New Zealand
        &[(20, 22), (65, 66), (70, 71)],
        // Row 27 (45-50°S): S. Patagonia, NZ south
        &[(20, 21), (70, 71)],
        // Row 28 (50-55°S): Tierra del Fuego
        &[(20, 21)],
        // Rows 29-35: Southern Ocean
        &[],
        &[],
        &[],
        &[],
        &[],
        &[],
        &[],
    ];

    let mut grid = [[false; GRID_COLS]; GRID_ROWS];
    for (r, row_ranges) in ranges.iter().enumerate() {
        for &(start, end) in *row_ranges {
            for c in start..=end.min(GRID_COLS - 1) {
                grid[r][c] = true;
            }
        }
    }
    grid
}

/// Check if a geographic coordinate falls on land using bilinear interpolation.
/// Returns a value between 0.0 and 1.0 (smoothed land probability).
pub fn land_value(grid: &LandGrid, lon: f64, lat: f64) -> f32 {
    let col_f = (lon + 180.0) / 5.0;
    let row_f = (90.0 - lat) / 5.0;

    let c0 = (col_f.floor() as usize).min(GRID_COLS - 1);
    let r0 = (row_f.floor() as usize).min(GRID_ROWS - 1);
    let c1 = (c0 + 1).min(GRID_COLS - 1);
    let r1 = (r0 + 1).min(GRID_ROWS - 1);

    let fx = (col_f - col_f.floor()) as f32;
    let fy = (row_f - row_f.floor()) as f32;

    let v = |r: usize, c: usize| -> f32 {
        if grid[r][c] {
            1.0
        } else {
            0.0
        }
    };

    let top = v(r0, c0) * (1.0 - fx) + v(r0, c1) * fx;
    let bot = v(r1, c0) * (1.0 - fx) + v(r1, c1) * fx;
    top * (1.0 - fy) + bot * fy
}

/// Server region classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Region {
    NaWest,
    NaEast,
    Sa,
    EuWest,
    EuEast,
    Af,
    As,
    Oc,
}

impl Region {
    pub fn label(&self) -> &'static str {
        match self {
            Region::NaWest => "NA WEST",
            Region::NaEast => "NA EAST",
            Region::Sa => "SA",
            Region::EuWest => "EU WEST",
            Region::EuEast => "EU EAST",
            Region::Af => "AF",
            Region::As => "AS",
            Region::Oc => "OC",
        }
    }

    /// Fixed label position on map (lon, lat)
    pub fn label_pos(&self) -> (f64, f64) {
        match self {
            Region::NaWest => (-130.0, 48.0),
            Region::NaEast => (-78.0, 48.0),
            Region::Sa => (-58.0, -18.0),
            Region::EuWest => (0.0, 58.0),
            Region::EuEast => (32.0, 55.0),
            Region::Af => (22.0, 0.0),
            Region::As => (110.0, 38.0),
            Region::Oc => (142.0, -30.0),
        }
    }

    pub fn all() -> &'static [Region] {
        &[
            Region::NaWest,
            Region::NaEast,
            Region::Sa,
            Region::EuWest,
            Region::EuEast,
            Region::Af,
            Region::As,
            Region::Oc,
        ]
    }
}

pub fn classify_region(lon: f64, lat: f64, desc: &str) -> Region {
    let d = desc.to_lowercase();

    // Oceania
    if d.contains("australia") || d.contains("zealand") || (lat < -10.0 && lon > 100.0) {
        return Region::Oc;
    }

    // South America
    if lon < -30.0 && lat < 15.0 {
        return Region::Sa;
    }

    // North America
    if lon < -25.0 && lat >= 15.0 {
        if lon < -105.0 {
            return Region::NaWest;
        }
        return Region::NaEast;
    }

    // Africa
    if d.contains("africa") || d.contains("johannesburg") {
        return Region::Af;
    }
    if lat < 30.0 && lat > -40.0 && lon > -20.0 && lon < 45.0 {
        return Region::Af;
    }

    // Europe
    if lat > 35.0 && lon >= -15.0 && lon < 40.0 {
        if lon < 20.0 {
            return Region::EuWest;
        }
        return Region::EuEast;
    }

    // Asia (everything else east)
    Region::As
}
