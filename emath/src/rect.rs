use std::f32::INFINITY;
use std::ops::RangeInclusive;

use crate::*;

/// A rectangular region of space.
///
/// Normally given in points, e.g. logical pixels.
#[derive(Clone, Copy, Eq, PartialEq, Default)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub struct Rect {
    pub min: Pos2,
    pub max: Pos2,
}

impl Rect {
    /// Infinite rectangle that contains everything.
    pub const EVERYTHING: Self = Self {
        min: pos2(-INFINITY, -INFINITY),
        max: pos2(INFINITY, INFINITY),
    };

    /// The inverse of [`Self::EVERYTHING`]: stretches from positive infinity to negative infinity.
    /// Contains no points.
    ///
    /// This is useful as the seed for bounding bounding boxes.
    ///
    /// ```
    /// # use emath::*;
    /// let inf = f32::INFINITY;
    /// assert!(Rect::NOTHING.size() == Vec2::splat(-inf));
    /// assert!(Rect::NOTHING.contains(pos2(0.0, 0.0)) == false);
    /// ```
    ///
    /// # Example:
    /// ```
    /// # use emath::*;
    /// let mut rect = Rect::NOTHING;
    /// rect.extend_with(pos2(2.0, 1.0));
    /// rect.extend_with(pos2(0.0, 3.0));
    /// assert_eq!(rect, Rect::from_min_max(pos2(0.0, 1.0), pos2(2.0, 3.0)))
    /// ```
    pub const NOTHING: Self = Self {
        min: pos2(INFINITY, INFINITY),
        max: pos2(-INFINITY, -INFINITY),
    };

    /// An invalid `Rect` filled with [`f32::NAN`];
    pub const NAN: Self = Self {
        min: pos2(f32::NAN, f32::NAN),
        max: pos2(-f32::NAN, -f32::NAN),
    };

    #[deprecated = "Use Rect::EVERYTHING"]
    pub fn everything() -> Self {
        let inf = f32::INFINITY;
        Self {
            min: pos2(-inf, -inf),
            max: pos2(inf, inf),
        }
    }

    #[deprecated = "Use Rect::NOTHING"]
    pub fn nothing() -> Self {
        let inf = f32::INFINITY;
        Self {
            min: pos2(inf, inf),
            max: pos2(-inf, -inf),
        }
    }

    #[deprecated = "Use Rect::NAN"]
    pub fn invalid() -> Self {
        Self::NAN
    }

    #[inline(always)]
    pub const fn from_min_max(min: Pos2, max: Pos2) -> Self {
        Rect { min, max }
    }

    pub fn from_min_size(min: Pos2, size: Vec2) -> Self {
        Rect {
            min,
            max: min + size,
        }
    }

    pub fn from_center_size(center: Pos2, size: Vec2) -> Self {
        Rect {
            min: center - size * 0.5,
            max: center + size * 0.5,
        }
    }

    pub fn from_x_y_ranges(x_range: RangeInclusive<f32>, y_range: RangeInclusive<f32>) -> Self {
        Rect {
            min: pos2(*x_range.start(), *y_range.start()),
            max: pos2(*x_range.end(), *y_range.end()),
        }
    }

    pub fn from_two_pos(a: Pos2, b: Pos2) -> Self {
        Rect {
            min: pos2(a.x.min(b.x), a.y.min(b.y)),
            max: pos2(a.x.max(b.x), a.y.max(b.y)),
        }
    }

    /// A `Rect` that contains every point to the right of the given X coordinate.
    pub fn everything_right_of(left_x: f32) -> Self {
        let mut rect = Self::EVERYTHING;
        rect.set_left(left_x);
        rect
    }

    /// A `Rect` that contains every point to the left of the given X coordinate.
    pub fn everything_left_of(right_x: f32) -> Self {
        let mut rect = Self::EVERYTHING;
        rect.set_right(right_x);
        rect
    }

    /// A `Rect` that contains every point below a certain y coordinate
    pub fn everything_below(top_y: f32) -> Self {
        let mut rect = Self::EVERYTHING;
        rect.set_top(top_y);
        rect
    }

    /// A `Rect` that contains every point above a certain y coordinate
    pub fn everything_above(bottom_y: f32) -> Self {
        let mut rect = Self::EVERYTHING;
        rect.set_bottom(bottom_y);
        rect
    }

    /// Expand by this much in each direction, keeping the center
    #[must_use]
    pub fn expand(self, amnt: f32) -> Self {
        self.expand2(Vec2::splat(amnt))
    }

    /// Expand by this much in each direction, keeping the center
    #[must_use]
    pub fn expand2(self, amnt: Vec2) -> Self {
        Rect::from_min_max(self.min - amnt, self.max + amnt)
    }

    /// Shrink by this much in each direction, keeping the center
    #[must_use]
    pub fn shrink(self, amnt: f32) -> Self {
        self.shrink2(Vec2::splat(amnt))
    }

    /// Shrink by this much in each direction, keeping the center
    #[must_use]
    pub fn shrink2(self, amnt: Vec2) -> Self {
        Rect::from_min_max(self.min + amnt, self.max - amnt)
    }

    #[must_use]
    pub fn translate(self, amnt: Vec2) -> Self {
        Rect::from_min_size(self.min + amnt, self.size())
    }

    /// The intersection of two `Rect`, i.e. the area covered by both.
    #[must_use]
    pub fn intersect(self, other: Rect) -> Self {
        Self {
            min: self.min.max(other.min),
            max: self.max.min(other.max),
        }
    }

    #[must_use]
    pub fn intersects(self, other: Rect) -> bool {
        self.min.x <= other.max.x
            && other.min.x <= self.max.x
            && self.min.y <= other.max.y
            && other.min.y <= self.max.y
    }

    /// keep min
    pub fn set_width(&mut self, w: f32) {
        self.max.x = self.min.x + w;
    }

    /// keep min
    pub fn set_height(&mut self, h: f32) {
        self.max.y = self.min.y + h;
    }

    /// Keep size
    pub fn set_center(&mut self, center: Pos2) {
        *self = self.translate(center - self.center());
    }

    #[must_use]
    #[inline(always)]
    pub fn contains(&self, p: Pos2) -> bool {
        self.min.x <= p.x && p.x <= self.max.x && self.min.y <= p.y && p.y <= self.max.y
    }

    #[must_use]
    pub fn contains_rect(&self, other: Rect) -> bool {
        self.contains(other.min) && self.contains(other.max)
    }

    /// Return the given points clamped to be inside the rectangle
    /// Panics if [`Self::is_negative`].
    #[must_use]
    pub fn clamp(&self, p: Pos2) -> Pos2 {
        p.clamp(self.min, self.max)
    }

    pub fn extend_with(&mut self, p: Pos2) {
        self.min = self.min.min(p);
        self.max = self.max.max(p);
    }

    /// Expand to include the given x coordinate
    pub fn extend_with_x(&mut self, x: f32) {
        self.min.x = self.min.x.min(x);
        self.max.x = self.max.x.max(x);
    }

    /// Expand to include the given y coordinate
    pub fn extend_with_y(&mut self, y: f32) {
        self.min.y = self.min.y.min(y);
        self.max.y = self.max.y.max(y);
    }

    pub fn union(self, other: Rect) -> Rect {
        Rect {
            min: self.min.min(other.min),
            max: self.max.max(other.max),
        }
    }

    #[inline(always)]
    pub fn center(&self) -> Pos2 {
        Pos2 {
            x: (self.min.x + self.max.x) / 2.0,
            y: (self.min.y + self.max.y) / 2.0,
        }
    }

    #[inline(always)]
    pub fn size(&self) -> Vec2 {
        self.max - self.min
    }

    #[inline(always)]
    pub fn width(&self) -> f32 {
        self.max.x - self.min.x
    }

    #[inline(always)]
    pub fn height(&self) -> f32 {
        self.max.y - self.min.y
    }

    /// Width / height
    ///
    /// * `aspect_ratio < 1`: portrait / high
    /// * `aspect_ratio = 1`: square
    /// * `aspect_ratio > 1`: landscape / wide
    pub fn aspect_ratio(&self) -> f32 {
        self.width() / self.height()
    }

    /// `[2, 1]` for wide screen, and `[1, 2]` for portrait, etc.
    /// At least one dimension = 1, the other >= 1
    /// Returns the proportions required to letter-box a square view area.
    pub fn square_proportions(&self) -> Vec2 {
        let w = self.width();
        let h = self.height();
        if w > h {
            vec2(w / h, 1.0)
        } else {
            vec2(1.0, h / w)
        }
    }

    pub fn area(&self) -> f32 {
        self.width() * self.height()
    }

    pub fn x_range(&self) -> RangeInclusive<f32> {
        self.min.x..=self.max.x
    }
    pub fn y_range(&self) -> RangeInclusive<f32> {
        self.min.y..=self.max.y
    }
    pub fn bottom_up_range(&self) -> RangeInclusive<f32> {
        self.max.y..=self.min.y
    }

    #[deprecated = "Use is_negative instead"]
    pub fn is_empty(&self) -> bool {
        self.max.x < self.min.x || self.max.y < self.min.y
    }

    /// `width < 0 || height < 0`
    #[inline(always)]
    pub fn is_negative(&self) -> bool {
        self.max.x < self.min.x || self.max.y < self.min.y
    }

    #[deprecated = "Use !is_negative() instead"]
    pub fn is_non_negative(&self) -> bool {
        !self.is_negative()
    }

    /// `width > 0 && height > 0`
    #[inline(always)]
    pub fn is_positive(&self) -> bool {
        self.min.x < self.max.x && self.min.y < self.max.y
    }

    /// True if all members are also finite.
    #[inline(always)]
    pub fn is_finite(&self) -> bool {
        self.min.is_finite() && self.max.is_finite()
    }

    /// True if any member is NaN.
    #[inline(always)]
    pub fn any_nan(self) -> bool {
        self.min.any_nan() || self.max.any_nan()
    }
}

/// ## Convenience functions (assumes origin is towards left top):
impl Rect {
    /// `min.x
    #[inline(always)]
    pub fn left(&self) -> f32 {
        self.min.x
    }
    /// `min.x`
    #[inline(always)]
    pub fn left_mut(&mut self) -> &mut f32 {
        &mut self.min.x
    }
    /// `min.x`
    #[inline(always)]
    pub fn set_left(&mut self, x: f32) {
        self.min.x = x;
    }

    /// `max.x`
    #[inline(always)]
    pub fn right(&self) -> f32 {
        self.max.x
    }
    /// `max.x`
    #[inline(always)]
    pub fn right_mut(&mut self) -> &mut f32 {
        &mut self.max.x
    }
    /// `max.x`
    #[inline(always)]
    pub fn set_right(&mut self, x: f32) {
        self.max.x = x;
    }

    /// `min.y`
    #[inline(always)]
    pub fn top(&self) -> f32 {
        self.min.y
    }
    /// `min.y`
    #[inline(always)]
    pub fn top_mut(&mut self) -> &mut f32 {
        &mut self.min.y
    }
    /// `min.y`
    #[inline(always)]
    pub fn set_top(&mut self, y: f32) {
        self.min.y = y;
    }

    /// `max.y`
    #[inline(always)]
    pub fn bottom(&self) -> f32 {
        self.max.y
    }
    /// `max.y`
    #[inline(always)]
    pub fn bottom_mut(&mut self) -> &mut f32 {
        &mut self.max.y
    }
    /// `max.y`
    #[inline(always)]
    pub fn set_bottom(&mut self, y: f32) {
        self.max.y = y;
    }

    #[inline(always)]
    pub fn left_top(&self) -> Pos2 {
        pos2(self.left(), self.top())
    }

    #[inline(always)]
    pub fn center_top(&self) -> Pos2 {
        pos2(self.center().x, self.top())
    }

    #[inline(always)]
    pub fn right_top(&self) -> Pos2 {
        pos2(self.right(), self.top())
    }

    #[inline(always)]
    pub fn left_center(&self) -> Pos2 {
        pos2(self.left(), self.center().y)
    }

    #[inline(always)]
    pub fn right_center(&self) -> Pos2 {
        pos2(self.right(), self.center().y)
    }

    #[inline(always)]
    pub fn left_bottom(&self) -> Pos2 {
        pos2(self.left(), self.bottom())
    }

    #[inline(always)]
    pub fn center_bottom(&self) -> Pos2 {
        pos2(self.center().x, self.bottom())
    }

    #[inline(always)]
    pub fn right_bottom(&self) -> Pos2 {
        pos2(self.right(), self.bottom())
    }
}

impl std::fmt::Debug for Rect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:?} - {:?}]", self.min, self.max)
    }
}

/// from (min, max) or (left top, right bottom)
impl From<[Pos2; 2]> for Rect {
    fn from([min, max]: [Pos2; 2]) -> Self {
        Self { min, max }
    }
}
