#![windows_subsystem = "windows"]

mod api;
mod app;
mod firewall;
mod map;
mod world_data;

fn load_icon() -> Option<egui::IconData> {
    let png_bytes = include_bytes!("../assets/icon.png");
    let img = image::load_from_memory(png_bytes).ok()?.into_rgba8();
    let (w, h) = img.dimensions();
    Some(egui::IconData {
        rgba: img.into_raw(),
        width: w,
        height: h,
    })
}

fn main() -> eframe::Result<()> {
    let mut viewport = egui::ViewportBuilder::default()
        .with_inner_size([1200.0, 700.0])
        .with_min_inner_size([800.0, 500.0])
        .with_title("CS2 Server Blocker by @ FPSHEAVEN");

    if let Some(icon) = load_icon() {
        viewport = viewport.with_icon(std::sync::Arc::new(icon));
    }

    let options = eframe::NativeOptions {
        viewport,
        ..Default::default()
    };

    eframe::run_native(
        "CS2 Server Blocker by @FPSHEAVEN",
        options,
        Box::new(|cc| Ok(Box::new(app::App::new(cc)))),
    )
}
