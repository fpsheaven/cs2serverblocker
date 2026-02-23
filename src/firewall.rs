use std::collections::HashSet;
use std::process::Command;

use crate::api::ServerLocation;

const RULE_PREFIX: &str = "CS2_Block";
const CREATE_NO_WINDOW: u32 = 0x08000000;

fn hidden_command(program: &str) -> Command {
    let mut cmd = Command::new(program);
    #[cfg(windows)]
    {
        use std::os::windows::process::CommandExt;
        cmd.creation_flags(CREATE_NO_WINDOW);
    }
    cmd
}

fn rule_name(code: &str) -> String {
    format!("{}_{}", RULE_PREFIX, code)
}

fn server_code_from_rule_name(name: &str) -> Option<String> {
    let rest = name.strip_prefix(RULE_PREFIX)?.strip_prefix('_')?;

    // Old format: CS2_Block_{code}_{a}_{b}_{c}_{d} (IP octets at end)
    let parts: Vec<&str> = rest.split('_').collect();
    if parts.len() >= 5 {
        let tail = &parts[parts.len() - 4..];
        if tail.iter().all(|p| p.parse::<u8>().is_ok()) {
            let code = parts[..parts.len() - 4].join("_");
            if !code.is_empty() {
                return Some(code);
            }
        }
    }

    // New format: CS2_Block_{code}
    if !rest.is_empty() {
        Some(rest.to_string())
    } else {
        None
    }
}

fn list_rule_names_via_powershell() -> Result<HashSet<String>, String> {
    let command = format!(
        "Get-NetFirewallRule -DisplayName '{}*' -ErrorAction SilentlyContinue | Select-Object -ExpandProperty DisplayName",
        RULE_PREFIX
    );

    let out = hidden_command("powershell")
        .args(["-NoProfile", "-Command", &command])
        .output()
        .map_err(|e| format!("Failed to run powershell: {}", e))?;

    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr);
        return Err(format!(
            "Failed to list firewall rules through PowerShell: {}",
            stderr.trim()
        ));
    }

    let mut names = HashSet::new();
    let text = String::from_utf8_lossy(&out.stdout);
    for line in text.lines() {
        let name = line.trim();
        if !name.is_empty() && name.starts_with(RULE_PREFIX) {
            names.insert(name.to_string());
        }
    }
    Ok(names)
}

fn list_rule_names_via_netsh() -> Result<HashSet<String>, String> {
    let out = hidden_command("netsh")
        .args(["advfirewall", "firewall", "show", "rule", "name=all"])
        .output()
        .map_err(|e| format!("Failed to run netsh: {}", e))?;

    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr);
        return Err(format!(
            "Failed to list firewall rules through netsh: {}",
            stderr.trim()
        ));
    }

    let mut names = HashSet::new();
    let text = String::from_utf8_lossy(&out.stdout);
    for line in text.lines() {
        if let Some(idx) = line.find(RULE_PREFIX) {
            let name = line[idx..].trim();
            if !name.is_empty() {
                names.insert(name.to_string());
            }
        }
    }
    Ok(names)
}

fn list_rule_names() -> Result<HashSet<String>, String> {
    list_rule_names_via_powershell().or_else(|_| list_rule_names_via_netsh())
}

/// Run a batch of netsh commands in a single PowerShell invocation.
/// Falls back to individual netsh calls if PowerShell fails to launch.
fn run_netsh_batch(commands: &[Vec<String>]) -> Result<(), String> {
    if commands.is_empty() {
        return Ok(());
    }

    // Build a PowerShell script that runs all netsh commands and collects errors.
    let mut script = String::from("$ErrorActionPreference='Stop'\n$failed=@()\n");
    for (i, args) in commands.iter().enumerate() {
        let args_str = args
            .iter()
            .map(|a| format!("'{}'", a.replace('\'', "''")))
            .collect::<Vec<_>>()
            .join(" ");
        script.push_str(&format!(
            "$r = & netsh {}\nif ($LASTEXITCODE -ne 0) {{ $failed += '#{}: ' + ($r -join ' ') }}\n",
            args_str, i
        ));
    }
    script.push_str("if ($failed.Count -gt 0) { Write-Error ($failed -join \"`n\"); exit 1 }\n");

    let out = hidden_command("powershell")
        .args(["-NoProfile", "-Command", &script])
        .output();

    match out {
        Ok(output) => {
            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                return Err(format!("Batch firewall operation failed: {}", stderr.trim()));
            }
            Ok(())
        }
        Err(_) => {
            // Fallback: run each netsh command individually
            for args in commands {
                let out = hidden_command("netsh")
                    .args(args)
                    .output()
                    .map_err(|e| format!("Failed to run netsh: {}", e))?;
                if !out.status.success() {
                    let stderr = String::from_utf8_lossy(&out.stderr);
                    return Err(format!("netsh failed: {}", stderr.trim()));
                }
            }
            Ok(())
        }
    }
}

/// Block multiple server locations in a single PowerShell invocation.
/// Combines all relay IPs per server into one rule, creating only 4 rules per server
/// (in/out × tcp/udp) regardless of relay count.
pub fn block_servers(locations: &[ServerLocation]) -> Result<(), String> {
    if locations.is_empty() {
        return Ok(());
    }

    let mut commands = Vec::new();
    for location in locations {
        let name = rule_name(&location.code);

        // Combine all relay IPs into a comma-separated list
        let remote_ip = location
            .relays
            .iter()
            .map(|r| r.ipv4.as_str())
            .collect::<Vec<_>>()
            .join(",");

        // Use the widest port range across all relays
        let port_low = location
            .relays
            .iter()
            .map(|r| r.port_range[0].min(r.port_range[1]))
            .min()
            .unwrap_or(0);
        let port_high = location
            .relays
            .iter()
            .map(|r| r.port_range[0].max(r.port_range[1]))
            .max()
            .unwrap_or(0);
        let port_arg = format!("remoteport={}-{}", port_low, port_high);

        for dir in ["in", "out"] {
            for protocol in ["udp", "tcp"] {
                commands.push(vec![
                    "advfirewall".into(),
                    "firewall".into(),
                    "add".into(),
                    "rule".into(),
                    format!("name={}", name),
                    format!("dir={}", dir),
                    "action=block".into(),
                    format!("remoteip={}", remote_ip),
                    format!("protocol={}", protocol),
                    port_arg.clone(),
                    "enable=yes".into(),
                ]);
            }
        }
    }

    run_netsh_batch(&commands)
}

/// Unblock multiple server locations in a single PowerShell invocation.
/// Handles both old (per-IP) and new (combined) rule formats.
pub fn unblock_servers(locations: &[ServerLocation]) -> Result<(), String> {
    if locations.is_empty() {
        return Ok(());
    }

    let existing_names = list_rule_names()?;

    // Collect codes we want to remove
    let target_codes: HashSet<&str> = locations.iter().map(|l| l.code.as_str()).collect();

    // Find all existing rule names that belong to our target servers
    let mut names_to_delete: Vec<&String> = Vec::new();
    for name in &existing_names {
        if let Some(code) = server_code_from_rule_name(name) {
            if target_codes.contains(code.as_str()) {
                names_to_delete.push(name);
            }
        }
    }

    if names_to_delete.is_empty() {
        return Ok(());
    }

    let mut commands = Vec::new();
    for name in &names_to_delete {
        commands.push(vec![
            "advfirewall".into(),
            "firewall".into(),
            "delete".into(),
            "rule".into(),
            format!("name={}", name),
        ]);
    }

    run_netsh_batch(&commands)
}

/// Get set of server codes that currently have firewall block rules
pub fn get_blocked_servers() -> HashSet<String> {
    let mut blocked = HashSet::new();

    if let Ok(rule_names) = list_rule_names() {
        for name in rule_names {
            if let Some(code) = server_code_from_rule_name(&name) {
                blocked.insert(code);
            }
        }
    }

    blocked
}

/// Check if running with administrator privileges
pub fn is_admin() -> bool {
    let output = hidden_command("net").args(["session"]).output();

    match output {
        Ok(out) => out.status.success(),
        Err(_) => false,
    }
}

/// Re-launch the application as administrator
pub fn relaunch_as_admin() -> Result<(), String> {
    let exe = std::env::current_exe().map_err(|e| format!("Failed to get exe path: {}", e))?;

    let exe_str = exe.to_string_lossy().to_string();

    #[cfg(windows)]
    {
        let status = hidden_command("powershell")
            .args([
                "-Command",
                &format!(
                    "Start-Process '{}' -Verb RunAs",
                    exe_str.replace('\'', "''")
                ),
            ])
            .status()
            .map_err(|e| format!("Failed to elevate: {}", e))?;

        if status.success() {
            std::process::exit(0);
        } else {
            return Err("User declined elevation".to_string());
        }
    }

    #[cfg(not(windows))]
    {
        Err("Administrator elevation only supported on Windows".to_string())
    }
}
