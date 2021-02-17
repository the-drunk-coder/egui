use egui::{CtxRef, ScrollArea, Ui, Window};

// ----------------------------------------------------------------------------

#[cfg_attr(feature = "persistence", derive(serde::Deserialize, serde::Serialize))]
#[cfg_attr(feature = "persistence", serde(default))]
struct Demos {
    open: Vec<bool>,

    #[cfg_attr(feature = "persistence", serde(skip))]
    demos: Vec<Box<dyn super::Demo>>,
}
impl Default for Demos {
    fn default() -> Self {
        let demos: Vec<Box<dyn super::Demo>> = vec![
            Box::new(super::dancing_strings::DancingStrings::default()),
            Box::new(super::drag_and_drop::DragAndDropDemo::default()),
            Box::new(super::font_book::FontBook::default()),
            Box::new(super::DemoWindow::default()),
            Box::new(super::painting::Painting::default()),
            Box::new(super::plot_demo::PlotDemo::default()),
            Box::new(super::scrolling::Scrolling::default()),
            Box::new(super::sliders::Sliders::default()),
            Box::new(super::widget_gallery::WidgetGallery::default()),
            Box::new(super::window_options::WindowOptions::default()),
            Box::new(super::tests::WindowResizeTest::default()),
            // Tests:
            Box::new(super::tests::IdTest::default()),
            Box::new(super::tests::InputTest::default()),
            Box::new(super::layout_test::LayoutTest::default()),
            Box::new(super::tests::ManualLayoutTest::default()),
            Box::new(super::tests::TableTest::default()),
        ];
        Self {
            open: vec![false; demos.len()],
            demos,
        }
    }
}
impl Demos {
    pub fn checkboxes(&mut self, ui: &mut Ui) {
        let Self { open, demos } = self;
        for (ref mut open, demo) in open.iter_mut().zip(demos.iter()) {
            ui.checkbox(open, demo.name());
        }
    }

    pub fn show(&mut self, ctx: &CtxRef) {
        let Self { open, demos } = self;
        open.resize(demos.len(), false); // Handle deserialization of old data.
        for (ref mut open, demo) in open.iter_mut().zip(demos.iter_mut()) {
            demo.show(ctx, open);
        }
    }
}

// ----------------------------------------------------------------------------

/// A menu bar in which you can select different demo windows to show.
#[derive(Default)]
#[cfg_attr(feature = "persistence", derive(serde::Deserialize, serde::Serialize))]
#[cfg_attr(feature = "persistence", serde(default))]
pub struct DemoWindows {
    open_windows: OpenWindows,

    demo_window: super::DemoWindow,

    /// open, title, view
    demos: Demos,
}

impl DemoWindows {
    /// Show the app ui (menu bar and windows).
    /// `sidebar_ui` can be used to optionally show some things in the sidebar
    pub fn ui(&mut self, ctx: &CtxRef) {
        egui::SidePanel::left("side_panel", 185.0).show(ctx, |ui| {
            ui.heading("✒ egui demos");

            ui.separator();

            ScrollArea::auto_sized().show(ui, |ui| {
                ui.label("egui is an immediate mode GUI library written in Rust.");
                ui.add(
                    egui::Hyperlink::new("https://github.com/emilk/egui").text(" egui home page"),
                );

                ui.label("egui can be run on the web, or natively on 🐧");

                ui.separator();

                ui.heading("Windows:");
                self.demos.checkboxes(ui);
                self.open_windows.checkboxes(ui);

                ui.separator();

                if ui.button("Organize windows").clicked() {
                    ui.ctx().memory().reset_areas();
                }
            });
        });

        egui::TopPanel::top("menu_bar").show(ctx, |ui| {
            show_menu_bar(ui);
        });

        // Just get a background to put the windows on instead of using whatever the clear color is
        let frame = egui::Frame {
            fill: ctx.style().visuals.extreme_bg_color,
            ..egui::Frame::none()
        };
        egui::CentralPanel::default().frame(frame).show(ctx, |_| {});

        self.windows(ctx);
    }

    /// Show the open windows.
    fn windows(&mut self, ctx: &CtxRef) {
        let Self {
            open_windows,
            demos,
            ..
        } = self;

        Window::new("🔧 Settings")
            .open(&mut open_windows.settings)
            .scroll(true)
            .show(ctx, |ui| {
                ctx.settings_ui(ui);
            });

        Window::new("🔍 Inspection")
            .open(&mut open_windows.inspection)
            .scroll(true)
            .show(ctx, |ui| {
                ctx.inspection_ui(ui);
            });

        Window::new("📝 Memory")
            .open(&mut open_windows.memory)
            .resizable(false)
            .show(ctx, |ui| {
                ctx.memory_ui(ui);
            });

        demos.show(ctx);
    }
}

// ----------------------------------------------------------------------------

#[cfg_attr(feature = "persistence", derive(serde::Deserialize, serde::Serialize))]
struct OpenWindows {
    // egui stuff:
    settings: bool,
    inspection: bool,
    memory: bool,
}

impl Default for OpenWindows {
    fn default() -> Self {
        OpenWindows::none()
    }
}

impl OpenWindows {
    fn none() -> Self {
        Self {
            settings: false,
            inspection: false,
            memory: false,
        }
    }

    fn checkboxes(&mut self, ui: &mut Ui) {
        let Self {
            settings,
            inspection,
            memory,
        } = self;

        ui.separator();
        ui.label("egui:");
        ui.checkbox(settings, "🔧 Settings");
        ui.checkbox(inspection, "🔍 Inspection");
        ui.checkbox(memory, "📝 Memory");
    }
}

fn show_menu_bar(ui: &mut Ui) {
    use egui::*;

    menu::bar(ui, |ui| {
        menu::menu(ui, "File", |ui| {
            if ui.button("Organize windows").clicked() {
                ui.ctx().memory().reset_areas();
            }
            if ui
                .button("Clear egui memory")
                .on_hover_text("Forget scroll, collapsing headers etc")
                .clicked()
            {
                *ui.ctx().memory() = Default::default();
            }
        });
    });
}
