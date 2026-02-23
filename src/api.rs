use std::{collections::HashMap, time::Duration};

use serde::Deserialize;

#[derive(Debug, Clone)]
pub struct Relay {
    pub ipv4: String,
    pub port_range: [u16; 2],
}

#[derive(Debug, Clone)]
pub struct ServerLocation {
    pub code: String,
    pub desc: String,
    pub longitude: f64,
    pub latitude: f64,
    pub relays: Vec<Relay>,
}

#[derive(Debug, Deserialize)]
struct SdrConfigResponse {
    pops: HashMap<String, PopData>,
}

#[derive(Debug, Deserialize)]
struct PopData {
    #[serde(default)]
    desc: Option<String>,
    #[serde(default)]
    geo: Option<Vec<f64>>,
    #[serde(default)]
    relays: Vec<RelayData>,
}

#[derive(Debug, Deserialize)]
struct RelayData {
    ipv4: String,
    #[serde(default)]
    port_range: Option<Vec<u64>>,
}

pub fn fetch_server_locations() -> Result<Vec<ServerLocation>, String> {
    let url = "https://api.steampowered.com/ISteamApps/GetSDRConfig/v1/?appid=730";

    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(10))
        .build()
        .map_err(|e| format!("Failed to build HTTP client: {}", e))?;

    let response = client
        .get(url)
        .send()
        .map_err(|e| format!("Failed to fetch server config: {}", e))?;

    let response = response
        .error_for_status()
        .map_err(|e| format!("Steam API returned an error: {}", e))?;

    let config: SdrConfigResponse = response
        .json()
        .map_err(|e| format!("Failed to parse JSON: {}", e))?;

    let mut locations = Vec::new();

    for (code, pop_data) in config.pops {
        let relays = pop_data
            .relays
            .into_iter()
            .filter_map(|relay| {
                let port_range = relay.port_range?;
                if port_range.len() < 2 {
                    return None;
                }

                let low = u16::try_from(port_range[0]).ok()?;
                let high = u16::try_from(port_range[1]).ok()?;
                let (low, high) = if low <= high {
                    (low, high)
                } else {
                    (high, low)
                };

                Some(Relay {
                    ipv4: relay.ipv4,
                    port_range: [low, high],
                })
            })
            .collect::<Vec<_>>();

        if relays.is_empty() {
            continue;
        }

        let (longitude, latitude) = match pop_data.geo {
            Some(geo) if geo.len() >= 2 => {
                let lon = geo[0];
                let lat = geo[1];
                if !(-180.0..=180.0).contains(&lon) || !(-90.0..=90.0).contains(&lat) {
                    continue;
                }
                (lon, lat)
            }
            _ => continue,
        };

        let desc = pop_data.desc.unwrap_or_else(|| code.clone());

        locations.push(ServerLocation {
            code,
            desc,
            longitude,
            latitude,
            relays,
        });
    }

    // Sort by description for consistent display
    locations.sort_by_key(|loc| loc.desc.to_lowercase());

    Ok(locations)
}
