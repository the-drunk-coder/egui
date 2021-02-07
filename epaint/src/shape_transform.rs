use crate::*;

pub fn adjust_colors(shape: &mut Shape, adjust_color: &impl Fn(&mut Color32)) {
    match shape {
        Shape::Noop => {}
        Shape::Vec(shapes) => {
            for shape in shapes {
                adjust_colors(shape, adjust_color)
            }
        }
        Shape::Circle { fill, stroke, .. } => {
            adjust_color(fill);
            adjust_color(&mut stroke.color);
        }
        Shape::LineSegment { stroke, .. } => {
            adjust_color(&mut stroke.color);
        }
        Shape::Path { fill, stroke, .. } => {
            adjust_color(fill);
            adjust_color(&mut stroke.color);
        }
        Shape::Rect { fill, stroke, .. } => {
            adjust_color(fill);
            adjust_color(&mut stroke.color);
        }
        Shape::Text { color, .. } => {
            adjust_color(color);
        }
	Shape::MulticolorText { colors, .. } => {
	    for color in colors.iter_mut() {
		adjust_color(color.1);
	    }            
        }
        Shape::Mesh(mesh) => {
            for v in &mut mesh.vertices {
                adjust_color(&mut v.color);
            }
        }
    }
}
