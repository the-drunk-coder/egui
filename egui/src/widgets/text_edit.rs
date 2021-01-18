use crate::{
    paint::{text::cursor::*, *},
    util::undoer::Undoer,
    *,
};

#[derive(Clone, Debug, Default)]
#[cfg_attr(feature = "persistence", derive(serde::Deserialize, serde::Serialize))]
#[cfg_attr(feature = "persistence", serde(default))]
pub(crate) struct State {
    cursorp: Option<CursorPair>,
    flash_cursorp: Option<CursorPair>,
    flash_alpha: u8,
    
    #[cfg_attr(feature = "persistence", serde(skip))]
    undoer: Undoer<(CCursorPair, String)>,
}

#[derive(Clone, Copy, Debug, Default)]
#[cfg_attr(feature = "persistence", derive(serde::Deserialize, serde::Serialize))]
struct CursorPair {
    /// When selecting with a mouse, this is where the mouse was released.
    /// When moving with e.g. shift+arrows, this is what moves.
    /// Note that the two ends can come in any order, and also be equal (no selection).
    pub primary: Cursor,

    /// When selecting with a mouse, this is where the mouse was first pressed.
    /// This part of the cursor does not move when shift is down.
    pub secondary: Cursor,
}

impl CursorPair {
    fn one(cursor: Cursor) -> Self {
        Self {
            primary: cursor,
            secondary: cursor,
        }
    }

    fn two(min: Cursor, max: Cursor) -> Self {
        Self {
            primary: max,
            secondary: min,
        }
    }

    fn as_ccursorp(&self) -> CCursorPair {
        CCursorPair {
            primary: self.primary.ccursor,
            secondary: self.secondary.ccursor,
        }
    }

    fn is_empty(&self) -> bool {
        self.primary.ccursor == self.secondary.ccursor
    }

    /// If there is a selection, None is returned.
    /// If the two ends is the same, that is returned.
    fn single(&self) -> Option<Cursor> {
        if self.is_empty() {
            Some(self.primary)
        } else {
            None
        }
    }

    fn primary_is_first(&self) -> bool {
        let p = self.primary.ccursor;
        let s = self.secondary.ccursor;
        (p.index, p.prefer_next_row) <= (s.index, s.prefer_next_row)
    }

    fn sorted(&self) -> [Cursor; 2] {
        if self.primary_is_first() {
            [self.primary, self.secondary]
        } else {
            [self.secondary, self.primary]
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
#[cfg_attr(feature = "persistence", derive(serde::Deserialize, serde::Serialize))]
struct CCursorPair {
    /// When selecting with a mouse, this is where the mouse was released.
    /// When moving with e.g. shift+arrows, this is what moves.
    /// Note that the two ends can come in any order, and also be equal (no selection).
    pub primary: CCursor,

    /// When selecting with a mouse, this is where the mouse was first pressed.
    /// This part of the cursor does not move when shift is down.
    pub secondary: CCursor,
}

impl CCursorPair {
    fn one(ccursor: CCursor) -> Self {
        Self {
            primary: ccursor,
            secondary: ccursor,
        }
    }

    fn two(min: CCursor, max: CCursor) -> Self {
        Self {
            primary: max,
            secondary: min,
        }
    }
}

/// A text region that the user can edit the contents of.
///
/// Example:
///
/// ```
/// # let mut ui = egui::Ui::__test();
/// # let mut my_string = String::new();
/// let response = ui.add(egui::TextEdit::singleline(&mut my_string));
/// if response.lost_kb_focus {
///     // use my_string
/// }
/// ```
#[must_use = "You should put this widget in an ui with `ui.add(widget);`"]
#[derive(Debug)]
pub struct TextEdit<'t> {
    text: &'t mut String,
    id: Option<Id>,
    id_source: Option<Id>,
    text_style: Option<TextStyle>,
    text_color: Option<Color32>,
    frame: bool,
    multiline: bool,
    enabled: bool,
    desired_width: Option<f32>,
    desired_height_rows: usize,
}

impl<'t> TextEdit<'t> {
    #[deprecated = "Use `TextEdit::singleline` or `TextEdit::multiline` (or the helper `ui.text_edit_singleline`, `ui.text_edit_multiline`) instead"]
    pub fn new(text: &'t mut String) -> Self {
        Self::multiline(text)
    }
    
    /// Now newlines (`\n`) allowed. Pressing enter key will result in the `TextEdit` loosing focus (`response.lost_kb_focus`).
    pub fn singleline(text: &'t mut String) -> Self {
        TextEdit {
            text,
            id: None,
            id_source: None,
            text_style: None,
            text_color: None,
            frame: true,
            multiline: false,
            enabled: true,
            desired_width: None,
            desired_height_rows: 1,
        }
    }

    /// A `TextEdit` for multiple lines. Pressing enter key will create a new line.
    pub fn multiline(text: &'t mut String) -> Self {
        TextEdit {
            text,
            id: None,
            id_source: None,
            text_style: None,
            frame: true,
            text_color: None,
            multiline: true,
            enabled: true,
            desired_width: None,
            desired_height_rows: 4,
        }
    }

    pub fn id(mut self, id: Id) -> Self {
        self.id = Some(id);
        self
    }

    /// A source for the unique `Id`, e.g. `.id_source("second_text_edit_field")` or `.id_source(loop_index)`.
    pub fn id_source(mut self, id_source: impl std::hash::Hash) -> Self {
        self.id_source = Some(Id::new(id_source));
        self
    }

    pub fn text_style(mut self, text_style: TextStyle) -> Self {
        self.text_style = Some(text_style);
        self
    }

    pub fn text_color(mut self, text_color: Color32) -> Self {
        self.text_color = Some(text_color);
        self
    }

    pub fn text_color_opt(mut self, text_color: Option<Color32>) -> Self {
        self.text_color = text_color;
        self
    }

    /// Default is `true`. If set to `false` then you cannot edit the text.
    pub fn enabled(mut self, enabled: bool) -> Self {
        self.enabled = enabled;
        self
    }

    /// Default is `true`. If set to `false` there will be no frame showing that this is editable text!
    pub fn frame(mut self, frame: bool) -> Self {
        self.frame = frame;
        self
    }

    /// Set to 0.0 to keep as small as possible
    pub fn desired_width(mut self, desired_width: f32) -> Self {
        self.desired_width = Some(desired_width);
        self
    }

    /// Set the number of rows to show by default.
    /// The default for singleline text is `1`.
    /// The default for multiline text is `4`.
    pub fn desired_rows(mut self, desired_height_rows: usize) -> Self {
        self.desired_height_rows = desired_height_rows;
        self
    }
}

impl<'t> Widget for TextEdit<'t> {
    fn ui(self, ui: &mut Ui) -> Response {
        let frame = self.frame;
        let where_to_put_background = ui.painter().add(Shape::Noop);

        let margin = Vec2::new(4.0, 2.0);
        let max_rect = ui.available_rect_before_wrap().shrink2(margin);
        let mut content_ui = ui.child_ui(max_rect, *ui.layout());
        let response = self.content_ui(&mut content_ui);
        let frame_rect = response.rect.expand2(margin);
        let response = response | ui.allocate_rect(frame_rect, Sense::click());

        if frame {
            let visuals = ui.style().interact(&response);
            let frame_rect = response.rect.expand(visuals.expansion);
            let shape = if response.has_kb_focus {
                Shape::Rect {
                    rect: frame_rect,
                    corner_radius: visuals.corner_radius,
                    // fill: ui.style().visuals.selection.bg_fill,
                    fill: ui.style().visuals.dark_bg_color,
                    stroke: ui.style().visuals.selection.stroke,
                }
            } else {
                Shape::Rect {
                    rect: frame_rect,
                    corner_radius: visuals.corner_radius,
                    fill: ui.style().visuals.dark_bg_color,
                    stroke: visuals.bg_stroke, // TODO: we want to show something here, or a text-edit field doesn't "pop".
                }
            };

            ui.painter().set(where_to_put_background, shape);
        }

        response
    }
}

impl<'t> TextEdit<'t> {
    fn content_ui(self, ui: &mut Ui) -> Response {
        let TextEdit {
            text,
            id,
            id_source,
            text_style,
            text_color,
            frame: _,
            multiline,
            enabled,
            desired_width,
            desired_height_rows,
        } = self;

        let text_style = text_style.unwrap_or_else(|| ui.style().body_text_style);
        let font = &ui.fonts()[text_style];
        let line_spacing = font.row_height();
        let available_width = ui.available_width();
        let mut galley = if multiline {
            font.layout_multiline(text.clone(), available_width)
        } else {
            font.layout_single_line(text.clone())
        };

        let desired_width = desired_width.unwrap_or_else(|| ui.style().spacing.text_edit_width);
        let desired_height = (desired_height_rows.at_least(1) as f32) * line_spacing;
        let desired_size = vec2(
            galley.size.x.max(desired_width.min(available_width)),
            galley.size.y.max(desired_height),
        );
        let (auto_id, rect) = ui.allocate_space(desired_size);

        let id = id.unwrap_or_else(|| {
            if let Some(id_source) = id_source {
                ui.make_persistent_id(id_source)
            } else {
                auto_id // Since we are only storing the cursor a persistent Id is not super important
            }
        });
        let mut state = ui.memory().text_edit.get(&id).cloned().unwrap_or_default();

        let sense = if enabled {
            Sense::click_and_drag()
        } else {
            Sense::hover()
        };
        let response = ui.interact(rect, id, sense);

        if enabled {
            ui.memory().interested_in_kb_focus(id);
        }

        if enabled {
            if let Some(mouse_pos) = ui.input().mouse.pos {
                // TODO: triple-click to select whole paragraph
                // TODO: drag selected text to either move or clone (ctrl on windows, alt on mac)

                let cursor_at_mouse = galley.cursor_from_pos(mouse_pos - response.rect.min);

                if response.hovered && ui.input().mouse.is_moving() {
                    // preview:
                    paint_cursor_end(ui, response.rect.min, &galley, &cursor_at_mouse);
                }

                if response.hovered && response.double_clicked {
                    // Select word:
                    let center = cursor_at_mouse;
                    let ccursorp = select_word_at(text, center.ccursor);
                    state.cursorp = Some(CursorPair {
                        primary: galley.from_ccursor(ccursorp.primary),
                        secondary: galley.from_ccursor(ccursorp.secondary),
                    });
                } else if response.hovered && ui.input().mouse.pressed {
                    ui.memory().request_kb_focus(id);
                    if ui.input().modifiers.shift {
                        if let Some(cursorp) = &mut state.cursorp {
                            cursorp.primary = cursor_at_mouse;
                        } else {
                            state.cursorp = Some(CursorPair::one(cursor_at_mouse));
                        }
                    } else {
                        state.cursorp = Some(CursorPair::one(cursor_at_mouse));
                    }
                } else if ui.input().mouse.down && response.active {
                    if let Some(cursorp) = &mut state.cursorp {
                        cursorp.primary = cursor_at_mouse;
                    }
                }
            }
        }

        if ui.input().mouse.pressed && !response.hovered {
            // User clicked somewhere else
            ui.memory().surrender_kb_focus(id);
        }

        if !enabled {
            ui.memory().surrender_kb_focus(id);
        }

        if response.hovered && enabled {
            ui.output().cursor_icon = CursorIcon::Text;
        }

        if ui.memory().has_kb_focus(id) && enabled {
            let mut cursorp = state
                .cursorp
                .map(|cursorp| {
                    // We only keep the PCursor (paragraph number, and character offset within that paragraph).
                    // This is so what if we resize the `TextEdit` region, and text wrapping changes,
                    // we keep the same byte character offset from the beginning of the text,
                    // even though the number of rows changes
                    // (each paragraph can be several rows, due to word wrapping).
                    // The column (character offset) should be able to extend beyond the last word so that we can
                    // go down and still end up on the same column when we return.
                    CursorPair {
                        primary: galley.from_pcursor(cursorp.primary.pcursor),
                        secondary: galley.from_pcursor(cursorp.secondary.pcursor),
                    }
                })
                .unwrap_or_else(|| CursorPair::one(galley.end()));

            // We feed state to the undoer both before and after handling input
            // so that the undoer creates automatic saves even when there are no events for a while.
            state
                .undoer
                .feed_state(ui.input().time, &(cursorp.as_ccursorp(), text.clone()));

            for event in &ui.input().events {
                let did_mutate_text = match event {
                    Event::Copy => {
                        if cursorp.is_empty() {
                            ui.ctx().output().copied_text = text.clone();
                        } else {
                            ui.ctx().output().copied_text = selected_str(text, &cursorp).to_owned();
                        }
                        None
                    }
                    Event::Cut => {
                        if cursorp.is_empty() {
                            ui.ctx().output().copied_text = std::mem::take(text);
                            Some(CCursorPair::default())
                        } else {
                            ui.ctx().output().copied_text = selected_str(text, &cursorp).to_owned();
                            Some(CCursorPair::one(delete_selected(text, &cursorp)))
                        }
                    }
                    Event::Text(text_to_insert) => {
                        // Newlines are handled by `Key::Enter`.
                        if !text_to_insert.is_empty()
                            && text_to_insert != "\n"
                            && text_to_insert != "\r"
                        {
                            let mut ccursor = delete_selected(text, &cursorp);
                            insert_text(&mut ccursor, text, text_to_insert);
                            Some(CCursorPair::one(ccursor))
                        } else {
                            None
                        }
                    }
                    Event::Key {
                        key: Key::Enter,
                        pressed: true,
                        ..
                    } => {
			if multiline {
                            let mut ccursor = delete_selected(text, &cursorp);
                            insert_text(&mut ccursor, text, "\n");
                            Some(CCursorPair::one(ccursor))
                        } else {
                            // Common to end input with enter
                            ui.memory().surrender_kb_focus(id);
                            break;
                        }
                    }
                    Event::Key {
                        key: Key::Escape,
                        pressed: true,
                        ..
                    } => {
                        ui.memory().surrender_kb_focus(id);
                        break;
                    }

                    Event::Key {
                        key: Key::Z,
                        pressed: true,
                        modifiers,
                    } if modifiers.command && !modifiers.shift => {
                        // TODO: redo
                        if let Some((undo_ccursorp, undo_txt)) =
                            state.undoer.undo(&(cursorp.as_ccursorp(), text.clone()))
                        {
                            *text = undo_txt.clone();
                            Some(*undo_ccursorp)
                        } else {
                            None
                        }
                    }

                    Event::Key {
                        key,
                        pressed: true,
                        modifiers,
                    } => on_key_press(&mut cursorp, text, &galley, *key, modifiers),

                    Event::Key { .. } => None,
                };

                if let Some(new_ccursorp) = did_mutate_text {
                    // Layout again to avoid frame delay, and to keep `text` and `galley` in sync.
                    let font = &ui.fonts()[text_style];
                    galley = if multiline {
                        font.layout_multiline(text.clone(), available_width)
                    } else {
                        font.layout_single_line(text.clone())
                    };

                    // Set cursorp using new galley:
                    cursorp = CursorPair {
                        primary: galley.from_ccursor(new_ccursorp.primary),
                        secondary: galley.from_ccursor(new_ccursorp.secondary),
                    };
                }
            }
            state.cursorp = Some(cursorp);

            state
                .undoer
                .feed_state(ui.input().time, &(cursorp.as_ccursorp(), text.clone()));
        }

        {
            //let visuals = ui.style().interact(&response);
            //let bg_rect = response.rect.expand(2.0); // breathing room for content
            //ui.painter().add(PaintCmd::Rect {
            //    rect: bg_rect,
            //    corner_radius: visuals.corner_radius,
            //    fill: ui.style().visuals.dark_bg_color,
                // fill: visuals.bg_fill,
            //    stroke: visuals.bg_stroke,
            //});
        }

        if ui.memory().has_kb_focus(id) {
            if let Some(cursorp) = state.cursorp {
                paint_cursor_selection(ui, response.rect.min, &galley, &cursorp, ui.style().visuals.selection.bg_fill);
                paint_cursor_end(ui, response.rect.min, &galley, &cursorp.primary);
            }
        }

        let text_color = text_color
            .or(ui.style().visuals.override_text_color)
            //.unwrap_or_else(|| ui.style().interact(&response).text_color()); // too bright
            .unwrap_or_else(|| ui.style().visuals.widgets.inactive.text_color());
        ui.painter()
            .galley(response.rect.min, galley, text_style, text_color);

        ui.memory().text_edit.insert(id, state);

        Response {
            lost_kb_focus: ui.memory().lost_kb_focus(id), // we may have lost it during the course of this function
            ..response
        }
    }
}

/// A text region that the user can edit the contents of, and call a
/// a callback on the selection on Ctrl+Enter
///
/// Example:
///
/// ```
/// # let mut ui = egui::Ui::__test();
/// # let mut my_string = String::new();
/// let response = ui.add(egui::TextEdit::singleline(&mut my_string));
/// if response.lost_kb_focus {
///     // use my_string
/// }
/// ```
use std::sync::{*, atomic::{Ordering, AtomicBool}};
use parking_lot::Mutex;

pub struct CallbackTextEdit<'t>  {
    text: &'t mut String,
    id: Option<Id>,
    id_source: Option<Id>,
    text_style: Option<TextStyle>,
    text_color: Option<Color32>,    
    enabled: bool,
    desired_width: Option<f32>,
    desired_height_rows: usize,
    eval_callback: Option<Arc<Mutex<dyn FnMut(&String)>>>,
    selection_toggle: &'t mut AtomicBool,
}

impl<'t> CallbackTextEdit<'t>  {
            
    /// A `TextEdit` for multiple lines. Pressing enter key will create a new line.
    pub fn multiline(text: &'t mut String, selection_toggle: &'t mut AtomicBool) -> Self {
        CallbackTextEdit {
            text,
            id: None,
            id_source: None,
            text_style: None,
            text_color: None,
            enabled: true,
            desired_width: None,
            desired_height_rows: 4,
	    eval_callback: None,
	    selection_toggle,
        }
    }

    pub fn id(mut self, id: Id) -> Self {
        self.id = Some(id);
        self
    }

    pub fn eval_callback(mut self, callback: &Arc<Mutex<dyn FnMut(&String)>>) -> Self {
        self.eval_callback = Some(Arc::clone(callback));
        self
    }

    /// A source for the unique `Id`, e.g. `.id_source("second_text_edit_field")` or `.id_source(loop_index)`.
    pub fn id_source(mut self, id_source: impl std::hash::Hash) -> Self {
        self.id_source = Some(Id::new(id_source));
        self
    }

    pub fn text_style(mut self, text_style: TextStyle) -> Self {
        self.text_style = Some(text_style);
        self
    }

    pub fn text_color(mut self, text_color: Color32) -> Self {
        self.text_color = Some(text_color);
        self
    }

    pub fn text_color_opt(mut self, text_color: Option<Color32>) -> Self {
        self.text_color = text_color;
        self
    }

    /// Default is `true`. If set to `false` then you cannot edit the text.
    pub fn enabled(mut self, enabled: bool) -> Self {
        self.enabled = enabled;
        self
    }

    /// Set to 0.0 to keep as small as possible
    pub fn desired_width(mut self, desired_width: f32) -> Self {
        self.desired_width = Some(desired_width);
        self
    }

    /// Set the number of rows to show by default.
    /// The default for singleline text is `1`.
    /// The default for multiline text is `4`.
    pub fn desired_rows(mut self, desired_height_rows: usize) -> Self {
        self.desired_height_rows = desired_height_rows;
        self
    }
}

impl<'t> Widget for CallbackTextEdit<'t>  {
    fn ui(self, ui: &mut Ui) -> Response {
        let CallbackTextEdit {
            text,
            id,
            id_source,
            text_style,
            text_color,
            enabled,
            desired_width,
            desired_height_rows,
	    eval_callback,
	    selection_toggle
        } = self;

        let text_style = text_style.unwrap_or_else(|| ui.style().body_text_style);
        let font = &ui.fonts()[text_style];
        let line_spacing = font.row_height();
        let available_width = ui.available_width();

	let mut galley = font.layout_multiline(text.clone(), available_width);
        
        let desired_width = desired_width.unwrap_or_else(|| ui.style().spacing.text_edit_width);
        let desired_height = (desired_height_rows.at_least(1) as f32) * line_spacing;
        let desired_size = vec2(
            galley.size.x.max(desired_width.min(available_width)),
            galley.size.y.max(desired_height),
        );

	let (auto_id, rect) = ui.allocate_space(desired_size);

        let id = id.unwrap_or_else(|| {
            if let Some(id_source) = id_source {
                ui.make_persistent_id(id_source)
            } else {
                auto_id // Since we are only storing the cursor, perfect persistence Id not super important
            }
        });
        
        let mut state = ui.memory().text_edit.get(&id).cloned().unwrap_or_default();

        let sense = if enabled {
            Sense::click_and_drag()
        } else {
            Sense::hover()
        };
        let response = ui.interact(rect, id, sense);
	
        if enabled {
            ui.memory().interested_in_kb_focus(id);
        }

        if enabled {
            if let Some(mouse_pos) = ui.input().mouse.pos {
                // TODO: triple-click to select whole paragraph
                // TODO: drag selected text to either move or clone (ctrl on windows, alt on mac)
		
                let cursor_at_mouse = galley.cursor_from_pos(mouse_pos - response.rect.min);

                if response.hovered {
                    // preview:
                    paint_cursor_end(ui, response.rect.min, &galley, &cursor_at_mouse);
                }

                if response.hovered && response.double_clicked {
                    // Select word:
                    let center = cursor_at_mouse;
                    let ccursorp = select_word_at(text, center.ccursor);
                    state.cursorp = Some(CursorPair {
                        primary: galley.from_ccursor(ccursorp.primary),
                        secondary: galley.from_ccursor(ccursorp.secondary),
                    });
                } else if response.hovered && ui.input().mouse.pressed {
                    ui.memory().request_kb_focus(id);
                    if ui.input().modifiers.shift {
                        if let Some(cursorp) = &mut state.cursorp {
                            cursorp.primary = cursor_at_mouse;
                        } else {
                            state.cursorp = Some(CursorPair::one(cursor_at_mouse));
                        }
                    } else {		
                        state.cursorp = Some(CursorPair::one(cursor_at_mouse));			
                    }
                } else if ui.input().mouse.down && response.active {
                    if let Some(cursorp) = &mut state.cursorp {
                        cursorp.primary = cursor_at_mouse;
                    }
                }
            }
        }

        if ui.input().mouse.pressed && !response.hovered {
            // User clicked somewhere else
            ui.memory().surrender_kb_focus(id);
        }

        if !enabled {
            ui.memory().surrender_kb_focus(id);
        }

        if response.hovered && enabled {
            ui.output().cursor_icon = CursorIcon::Text;
        }

        if ui.memory().has_kb_focus(id) && enabled {
            let mut cursorp = state
                .cursorp
                .map(|cursorp| {
                    // We only keep the PCursor (paragraph number, and character offset within that paragraph).
                    // This is so what if we resize the `TextEdit` region, and text wrapping changes,
                    // we keep the same byte character offset from the beginning of the text,
                    // even though the number of rows changes
                    // (each paragraph can be several rows, due to word wrapping).
                    // The column (character offset) should be able to extend beyond the last word so that we can
                    // go down and still end up on the same column when we return.
                    CursorPair {
                        primary: galley.from_pcursor(cursorp.primary.pcursor),
                        secondary: galley.from_pcursor(cursorp.secondary.pcursor),
                    }
                })
                .unwrap_or_else(|| CursorPair::one(galley.end()));
	    
            // We feed state to the undoer both before and after handling input
            // so that the undoer creates automatic saves even when there are no events for a while.
            state
                .undoer
                .feed_state(ui.input().time, &(cursorp.as_ccursorp(), text.clone()));	    
	    
            for event in &ui.input().events {
		let did_mutate_text = match event {
                    Event::Copy => {
			// clear selection
			selection_toggle.store(false, Ordering::SeqCst);

			// don't copy empty text
			if !cursorp.is_empty() {                            
                            ui.ctx().output().copied_text = selected_str(text, &cursorp).to_owned();
                        }
			
                        None
                    }
                    Event::Cut => {
			// clear selection
			selection_toggle.store(false, Ordering::SeqCst);
			
			if !cursorp.is_empty() {
                            ui.ctx().output().copied_text = selected_str(text, &cursorp).to_owned();
                            Some(CCursorPair::one(delete_selected(text, &cursorp)))
                        } else {
			    None
			}
                    }
                    Event::Text(text_to_insert) => {
			// clear selection
			selection_toggle.store(false, Ordering::SeqCst);
			
			// Newlines are handled by `Key::Enter`.
                        if !text_to_insert.is_empty()
                            && text_to_insert != "\n"
                            && text_to_insert != "\r"
                        {
                            let mut ccursor = delete_selected(text, &cursorp);
                            insert_text(&mut ccursor, text, text_to_insert);
                            Some(CCursorPair::one(ccursor))
                        } else {
                            None
                        }			
                    }
		    Event::Key {
                        key: Key::Tab,
                        pressed: true,
                        ..
                    } => {
			//println!("tab");
			let mut ccursor = delete_selected(text, &cursorp);
                        insert_text(&mut ccursor, text, "  ");
			// clear selection
			selection_toggle.store(false, Ordering::SeqCst);
			Some(CCursorPair::one(ccursor))			
		    }
		    Event::Key {
                        key: Key::Space,
                        pressed: true,
                        modifiers,
                    } => {
			if modifiers.command {
			    let sel = selection_toggle.load(Ordering::SeqCst);			   			    
			    selection_toggle.store(!sel, Ordering::SeqCst);			    
			}
			None
		    },
		    Event::Key {
                        key: Key::LParen, // electric parenthesis for s-expression languages ...
                        pressed: true,
                        ..
                    } => {
			// enclose selection in parenthesis and
			// jump to opening ...
			let selection = selected_str(text, &cursorp).clone().to_string();
			let selection_len = selection.len();
			let mut ccursor = delete_selected(text, &cursorp);
			insert_text(&mut ccursor, text, format!("({})", selection).as_str());
			// clear selection
			selection_toggle.store(false, Ordering::SeqCst);
			// go to opening paren so the function name can be entered ... 
			ccursor.index -= selection_len + 1;			    
			Some(CCursorPair::one(ccursor))
		    }
		    Event::Key {
                        key: Key::DoubleQuote, // electric parenthesis for s-expression languages ...
                        pressed: true,
                        ..
                    } => {
			// enclose selection in parenthesis and
			// jump to opening ...
			let selection = selected_str(text, &cursorp).clone().to_string();
			let selection_len = selection.len();
			let mut ccursor = delete_selected(text, &cursorp);
			insert_text(&mut ccursor, text, format!("\"{}\"", selection).as_str());
			// clear selection
			selection_toggle.store(false, Ordering::SeqCst);
			// go to opening paren so the function name can be entered ... 
			ccursor.index -= selection_len + 1;			    
			Some(CCursorPair::one(ccursor))
		    }	
                    Event::Key {
                        key: Key::Enter,
                        pressed: true,
                        modifiers,
                    } => {
			// clear selection
			selection_toggle.store(false, Ordering::SeqCst);
			if modifiers.command {			    
			    if let Some(sexp_cursors) = find_toplevel_sexp(text, &cursorp) {
				let cup = CursorPair {
				    primary: galley.from_ccursor(sexp_cursors.primary),
				    secondary: galley.from_ccursor(sexp_cursors.secondary),
				};
				let sel = selected_str(text, &cup);
				state.flash_cursorp = Some(cup);
				state.flash_alpha = 240; // set flash alpha ()
				if let Some(cb) = eval_callback {
				    let mut cb_loc = cb.lock();
				    cb_loc(&sel.to_string());
				} else {
				    println!("no callback!");
				}				    
			    }			    			    
			    break; // need to break here because of callback move ...
			} else {
                            let mut ccursor = delete_selected(text, &cursorp);
                            insert_text(&mut ccursor, text, "\n");
                            Some(CCursorPair::one(ccursor))
                        }			
                    }
                    Event::Key {
                        key: Key::Escape,
                        pressed: true,
                        ..
                    } => {
			// clear selection
			selection_toggle.store(false, Ordering::SeqCst);
			cursorp.secondary = cursorp.primary;
                        //ui.memory().surrender_kb_focus(id);			
                        break;
                    }

                    Event::Key {
                        key: Key::Z,
                        pressed: true,
                        modifiers,
                    } if modifiers.command && !modifiers.shift => {
                        // TODO: redo
                        if let Some((undo_ccursorp, undo_txt)) =
                            state.undoer.undo(&(cursorp.as_ccursorp(), text.clone()))
                        {
                            *text = undo_txt.clone();
                            Some(*undo_ccursorp)
                        } else {
                            None
                        }
                    }
		    
		    Event::Key {
                        key: Key::ArrowLeft,
                        pressed: true,
                        modifiers,
                    } => {
			move_single_cursor(&mut cursorp.primary, &galley, Key::ArrowLeft, modifiers, false);
			if !modifiers.shift && !selection_toggle.load(Ordering::SeqCst) {
			    cursorp.secondary = cursorp.primary;
			}
			None
		    }
		    Event::Key {
                        key: Key::ArrowRight,
                        pressed: true,
                        modifiers,
                    } => {
			move_single_cursor(&mut cursorp.primary, &galley, Key::ArrowRight, modifiers, false);
			if !modifiers.shift && !selection_toggle.load(Ordering::SeqCst) {
			    cursorp.secondary = cursorp.primary;
			}
			None
		    }
		    Event::Key {
                        key: Key::F,
                        pressed: true,
                        modifiers,
                    } => {
			if modifiers.ctrl {			    
			    move_single_cursor(&mut cursorp.primary, &galley, Key::ArrowRight, modifiers, true);
			    if !modifiers.shift && !selection_toggle.load(Ordering::SeqCst) {
				cursorp.secondary = cursorp.primary;
			    }
			}
			None
		    }
		    Event::Key {
                        key: Key::ArrowUp,
                        pressed: true,
                        modifiers,
                    } => {
			move_single_cursor(&mut cursorp.primary, &galley, Key::ArrowUp, modifiers, false);
			if !modifiers.shift && !selection_toggle.load(Ordering::SeqCst) {
			    cursorp.secondary = cursorp.primary;
			}
			None
		    }
		    Event::Key {
                        key: Key::ArrowDown,
                        pressed: true,
                        modifiers,
                    } => {
			move_single_cursor(&mut cursorp.primary, &galley, Key::ArrowDown, modifiers, false);
			if !modifiers.shift && !selection_toggle.load(Ordering::SeqCst) {
			    cursorp.secondary = cursorp.primary;
			}
			None
		    }
		    Event::Key {
                        key: Key::Home,
                        pressed: true,
                        modifiers,
                    } => {
			move_single_cursor(&mut cursorp.primary, &galley, Key::Home, modifiers, false);
			if !modifiers.shift && !selection_toggle.load(Ordering::SeqCst) {
			    cursorp.secondary = cursorp.primary;
			}
			None
		    }
		    Event::Key {
                        key: Key::End,
                        pressed: true,
                        modifiers,
                    } => {
			move_single_cursor(&mut cursorp.primary, &galley, Key::End, modifiers, false);
			if !modifiers.shift && !selection_toggle.load(Ordering::SeqCst) {
			    cursorp.secondary = cursorp.primary;
			}
			None
		    }
		    
                    Event::Key {
                        key,
                        pressed: true,
                        modifiers,
                    } => on_key_press(&mut cursorp, text, &galley, *key, modifiers),

                    Event::Key { .. } => None,
                };

                if let Some(new_ccursorp) = did_mutate_text {
                    // Layout again to avoid frame delay, and to keep `text` and `galley` in sync.
                    let font = &ui.fonts()[text_style];
                    galley = font.layout_multiline(text.clone(), available_width);

                    // Set cursorp using new galley:
                    cursorp = CursorPair {
                        primary: galley.from_ccursor(new_ccursorp.primary),
                        secondary: galley.from_ccursor(new_ccursorp.secondary),
                    };
                }
            }
            state.cursorp = Some(cursorp);

            state
                .undoer
                .feed_state(ui.input().time, &(cursorp.as_ccursorp(), text.clone()));
        }

        if ui.memory().has_kb_focus(id) {
            if let Some(cursorp) = state.cursorp {
                paint_cursor_selection(ui, response.rect.min, &galley, &cursorp, ui.style().visuals.selection.bg_fill);
                paint_cursor_end(ui, response.rect.min, &galley, &cursorp.primary);
            }
	    if let Some(cursorp) = state.flash_cursorp {
		if state.flash_alpha > 40 {
		    paint_cursor_selection(ui, response.rect.min, &galley, &cursorp, Color32::from_rgba_unmultiplied(220, 80, 20, state.flash_alpha));
		    state.flash_alpha -= 40;
		}
	    }
        }

        let text_color = text_color
            .or(ui.style().visuals.override_text_color)
            // .unwrap_or_else(|| ui.style().interact(&response).text_color()); // too bright
            .unwrap_or_else(|| ui.style().visuals.widgets.inactive.text_color());

	ui.painter()
            .galley(response.rect.min, galley, text_style, text_color);

        ui.memory().text_edit.insert(id, state);

        Response {
            lost_kb_focus: ui.memory().lost_kb_focus(id), // we may have lost it during the course of this function
            ..response
        }
    }
}

// ----------------------------------------------------------------------------

fn paint_cursor_selection(ui: &mut Ui, pos: Pos2, galley: &Galley, cursorp: &CursorPair, color: Color32) {
    if cursorp.is_empty() {
        return;
    }
    let [min, max] = cursorp.sorted();
    let min = min.rcursor;
    let max = max.rcursor;

    for ri in min.row..=max.row {
        let row = &galley.rows[ri];
        let left = if ri == min.row {
            row.x_offset(min.column)
        } else {
            row.min_x()
        };
        let right = if ri == max.row {
            row.x_offset(max.column)
        } else {
            let newline_size = if row.ends_with_newline {
                row.height() / 2.0 // visualize that we select the newline
            } else {
                0.0
            };
            row.max_x() + newline_size
        };
        let rect = Rect::from_min_max(pos + vec2(left, row.y_min), pos + vec2(right, row.y_max));
        ui.painter().rect_filled(rect, 0.0, color);
    }
}

fn paint_cursor_end(ui: &mut Ui, pos: Pos2, galley: &Galley, cursor: &Cursor) {
    let stroke = ui.style().visuals.selection.stroke;

    let cursor_pos = galley.pos_from_cursor(cursor).translate(pos.to_vec2());
    let cursor_pos = cursor_pos.expand(1.5); // slightly above/below row

    let top = cursor_pos.center_top();
    let bottom = cursor_pos.center_bottom();

    ui.painter().line_segment(
        [top, bottom],
        (ui.style().visuals.text_cursor_width, stroke.color),
    );

    if false {
        // Roof/floor:
        let extrusion = 3.0;
        let width = 1.0;
        ui.painter().line_segment(
            [top - vec2(extrusion, 0.0), top + vec2(extrusion, 0.0)],
            (width, stroke.color),
        );
        ui.painter().line_segment(
            [bottom - vec2(extrusion, 0.0), bottom + vec2(extrusion, 0.0)],
            (width, stroke.color),
        );
    }
}

// ----------------------------------------------------------------------------

fn selected_str<'s>(text: &'s str, cursorp: &CursorPair) -> &'s str {
    let [min, max] = cursorp.sorted();
    let byte_begin = byte_index_from_char_index(text, min.ccursor.index);
    let byte_end = byte_index_from_char_index(text, max.ccursor.index);
    &text[byte_begin..byte_end]
}

fn byte_index_from_char_index(s: &str, char_index: usize) -> usize {
    for (ci, (bi, _)) in s.char_indices().enumerate() {
        if ci == char_index {
            return bi;
        }
    }
    s.len()
}

fn insert_text(ccursor: &mut CCursor, text: &mut String, text_to_insert: &str) {
    let mut char_it = text.chars();
    let mut new_text = String::with_capacity(text.len() + text_to_insert.len());
    for _ in 0..ccursor.index {
        let c = char_it.next().unwrap();
        new_text.push(c);
    }
    ccursor.index += text_to_insert.chars().count();
    new_text += text_to_insert;
    new_text.extend(char_it);
    *text = new_text;
}

// ----------------------------------------------------------------------------

fn delete_selected(text: &mut String, cursorp: &CursorPair) -> CCursor {
    let [min, max] = cursorp.sorted();
    delete_selected_ccursor_range(text, [min.ccursor, max.ccursor])
}

fn delete_selected_ccursor_range(text: &mut String, [min, max]: [CCursor; 2]) -> CCursor {
    let [min, max] = [min.index, max.index];
    assert!(min <= max);
    if min < max {
        let mut char_it = text.chars();
        let mut new_text = String::with_capacity(text.len());
        for _ in 0..min {
            new_text.push(char_it.next().unwrap())
        }
        new_text.extend(char_it.skip(max - min));
        *text = new_text;
    }
    CCursor {
        index: min,
        prefer_next_row: true,
    }
}

fn delete_previous_char(text: &mut String, ccursor: CCursor) -> CCursor {
    if ccursor.index > 0 {
        let max_ccursor = ccursor;
        let min_ccursor = max_ccursor - 1;
        delete_selected_ccursor_range(text, [min_ccursor, max_ccursor])
    } else {
        ccursor
    }
}

fn delete_next_char(text: &mut String, ccursor: CCursor) -> CCursor {
    delete_selected_ccursor_range(text, [ccursor, ccursor + 1])
}

fn delete_previous_word(text: &mut String, max_ccursor: CCursor) -> CCursor {
    let min_ccursor = ccursor_previous_word(text, max_ccursor);
    delete_selected_ccursor_range(text, [min_ccursor, max_ccursor])
}

fn delete_next_word(text: &mut String, min_ccursor: CCursor) -> CCursor {
    let max_ccursor = ccursor_next_word(text, min_ccursor);
    delete_selected_ccursor_range(text, [min_ccursor, max_ccursor])
}

fn delete_paragraph_before_cursor(
    text: &mut String,
    galley: &Galley,
    cursorp: &CursorPair,
) -> CCursor {
    let [min, max] = cursorp.sorted();
    let min = galley.from_pcursor(PCursor {
        paragraph: min.pcursor.paragraph,
        offset: 0,
        prefer_next_row: true,
    });
    if min.ccursor == max.ccursor {
        delete_previous_char(text, min.ccursor)
    } else {
        delete_selected(text, &CursorPair::two(min, max))
    }
}

fn delete_paragraph_after_cursor(
    text: &mut String,
    galley: &Galley,
    cursorp: &CursorPair,
) -> CCursor {
    let [min, max] = cursorp.sorted();
    let max = galley.from_pcursor(PCursor {
        paragraph: max.pcursor.paragraph,
        offset: usize::MAX, // end of paragraph
        prefer_next_row: false,
    });
    if min.ccursor == max.ccursor {
        delete_next_char(text, min.ccursor)
    } else {
        delete_selected(text, &CursorPair::two(min, max))
    }
}

// ----------------------------------------------------------------------------

/// Returns `Some(new_cursor)` if we did mutate `text`.
fn on_key_press(
    cursorp: &mut CursorPair,
    text: &mut String,
    galley: &Galley,
    key: Key,
    modifiers: &Modifiers,    
) -> Option<CCursorPair> {
    match key {
        Key::Backspace => {
            let ccursor = if modifiers.mac_cmd {
                delete_paragraph_before_cursor(text, galley, cursorp)
            } else if let Some(cursor) = cursorp.single() {
                if modifiers.alt || modifiers.ctrl {
                    // alt on mac, ctrl on windows
                    delete_previous_word(text, cursor.ccursor)
                } else {
                    delete_previous_char(text, cursor.ccursor)
                }
            } else {
                delete_selected(text, cursorp)
            };
            Some(CCursorPair::one(ccursor))
        }
        Key::Delete => {
            let ccursor = if modifiers.mac_cmd {
                delete_paragraph_after_cursor(text, galley, cursorp)
            } else if let Some(cursor) = cursorp.single() {
                if modifiers.alt || modifiers.ctrl {
                    // alt on mac, ctrl on windows
                    delete_next_word(text, cursor.ccursor)
                } else {
                    delete_next_char(text, cursor.ccursor)
                }
            } else {
                delete_selected(text, cursorp)
            };
            let ccursor = CCursor {
                prefer_next_row: true,
                ..ccursor
            };
            Some(CCursorPair::one(ccursor))
        }
	
        Key::A if modifiers.command => {
            // select all
            *cursorp = CursorPair::two(Cursor::default(), galley.end());
            None
        }

        Key::K if modifiers.ctrl => {
            let ccursor = delete_paragraph_after_cursor(text, galley, cursorp);
            Some(CCursorPair::one(ccursor))
        }

        Key::U if modifiers.ctrl => {
            let ccursor = delete_paragraph_before_cursor(text, galley, cursorp);
            Some(CCursorPair::one(ccursor))
        }
        
        Key::ArrowLeft | Key::ArrowRight | Key::ArrowUp | Key::ArrowDown | Key::Home | Key::End => {
            move_single_cursor(&mut cursorp.primary, galley, key, modifiers, false);
            if !modifiers.shift {
                cursorp.secondary = cursorp.primary;
            }
            None
        }

        _ => None,
    }
}

fn move_single_cursor(cursor: &mut Cursor, galley: &Galley, key: Key, modifiers: &Modifiers, clear_modifiers: bool) {
    match key {
        Key::ArrowLeft => {
            if modifiers.alt || modifiers.ctrl {
                // alt on mac, ctrl on windows
                *cursor = galley.from_ccursor(ccursor_previous_word(&galley.text, cursor.ccursor));
            } else if modifiers.mac_cmd {
                *cursor = galley.cursor_begin_of_row(cursor);
            } else {
                *cursor = galley.cursor_left_one_character(cursor);
            }
        }
        Key::ArrowRight => {
            if !clear_modifiers && (modifiers.alt || modifiers.ctrl) {
                // alt on mac, ctrl on windows
                *cursor = galley.from_ccursor(ccursor_next_word(&galley.text, cursor.ccursor));
            } else if !clear_modifiers && modifiers.mac_cmd {
                *cursor = galley.cursor_end_of_row(cursor);
            } else {
                *cursor = galley.cursor_right_one_character(cursor);
            }
        }
        Key::ArrowUp => {
            if modifiers.command {
                // mac and windows behavior
                *cursor = Cursor::default();
            } else {
                *cursor = galley.cursor_up_one_row(cursor);
            }
        }
        Key::ArrowDown => {
            if modifiers.command {
                // mac and windows behavior
                *cursor = galley.end();
            } else {
                *cursor = galley.cursor_down_one_row(cursor);
            }
        }

        Key::Home => {
            if modifiers.ctrl {
                // windows behavior
                *cursor = Cursor::default();
            } else {
                *cursor = galley.cursor_begin_of_row(cursor);
            }
        }
        Key::End => {
            if modifiers.ctrl {
                // windows behavior
                *cursor = galley.end();
            } else {
                *cursor = galley.cursor_end_of_row(cursor);
            }
        }

        _ => unreachable!(),
    }
}

// ----------------------------------------------------------------------------

fn select_word_at(text: &str, ccursor: CCursor) -> CCursorPair {
    if ccursor.index == 0 {
        CCursorPair::two(ccursor, ccursor_next_word(text, ccursor))
    } else {
        let it = text.chars();
        let mut it = it.skip(ccursor.index - 1);
        if let Some(char_before_cursor) = it.next() {
            if let Some(char_after_cursor) = it.next() {
                if is_word_char(char_before_cursor) && is_word_char(char_after_cursor) {
                    let min = ccursor_previous_word(text, ccursor + 1);
                    let max = ccursor_next_word(text, min);
                    CCursorPair::two(min, max)
                } else if is_word_char(char_before_cursor) {
                    let min = ccursor_previous_word(text, ccursor);
                    let max = ccursor_next_word(text, min);
                    CCursorPair::two(min, max)
                } else if is_word_char(char_after_cursor) {
                    let max = ccursor_next_word(text, ccursor);
                    CCursorPair::two(ccursor, max)
                } else {
                    let min = ccursor_previous_word(text, ccursor);
                    let max = ccursor_next_word(text, ccursor);
                    CCursorPair::two(min, max)
                }
            } else {
                let min = ccursor_previous_word(text, ccursor);
                CCursorPair::two(min, ccursor)
            }
        } else {
            let max = ccursor_next_word(text, ccursor);
            CCursorPair::two(ccursor, max)
        }
    }
}

fn find_toplevel_sexp(text: &str, cursorp: &CursorPair) -> Option<CCursorPair> {
    let [min, _] = cursorp.sorted();
    
    let pos = min.ccursor.index;
    let rev_pos = text.len() - pos;
    
    let mut it_l = text.chars().rev();
    let mut it_r = text.chars();

    let mut l_pos = pos;
    let mut r_pos = pos;
    let mut last_closing = pos;
    let mut last_opening = pos;
  	
    for _ in 0..rev_pos {
	it_l.next();
    }

    for _ in 0..pos {	
	it_r.next();
    }

    let mut balance:i32 = 0;
    let mut newline_found = false;
    while let Some(l_char) = it_l.next() {
	if l_char == '\n' && newline_found { // two newlines - assume end
	    break;
	} else if l_char == '\n' {
	    l_pos -= 1;
	    newline_found = true;	    
	} else if l_char == '(' {	    
	    l_pos -= 1;
	    last_opening = l_pos;
	    balance += 1;
	    newline_found = false;
	} else if l_char == ')' {	    
	    l_pos -= 1;
	    balance -= 1;
	    newline_found = false;	    
	} else {
	    l_pos -= 1;
	    newline_found = false;	    
	}
    }

    newline_found = false;
    while let Some(r_char) = it_r.next() {
	if r_char == '\n' && newline_found { // two newlines - assume end
	    break;
	} else if r_char == '\n' {
	    r_pos += 1;
	    newline_found = true;	    
	} else if r_char == '(' {	    
	    r_pos += 1;
	    balance += 1;
	    newline_found = false;	    
	} else if r_char == ')' {	    
	    r_pos += 1;
	    last_closing = r_pos;
	    balance -= 1;
	    newline_found = false;	    
	} else {
	    r_pos += 1;
	    newline_found = false;	    
	}	
    }

    if balance == 0 {
	let left = CCursor {
            index: last_opening,
            prefer_next_row: true,
	};
	let right = CCursor {
            index: last_closing,
            prefer_next_row: false,
	};
	Some(CCursorPair::two(right, left))
    } else {
	None
    }
}

fn ccursor_next_word(text: &str, ccursor: CCursor) -> CCursor {
    CCursor {
        index: next_word_boundary_char_index(text.chars(), ccursor.index),
        prefer_next_row: false,
    }
}

fn ccursor_previous_word(text: &str, ccursor: CCursor) -> CCursor {
    let num_chars = text.chars().count();
    CCursor {
        index: num_chars
            - next_word_boundary_char_index(text.chars().rev(), num_chars - ccursor.index),
        prefer_next_row: true,
    }
}

fn next_word_boundary_char_index(it: impl Iterator<Item = char>, mut index: usize) -> usize {
    let mut it = it.skip(index);
    if let Some(_first) = it.next() {
        index += 1;

        if let Some(second) = it.next() {
            index += 1;
            for next in it {
                if is_word_char(next) != is_word_char(second) {
                    break;
                }
                index += 1;
            }
        }
    }
    index
}

fn is_word_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}
