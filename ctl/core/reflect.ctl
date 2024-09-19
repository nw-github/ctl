// i*, u*, f32, f64, int, uint, c_*
@(lang(numeric))
pub sealed trait Numeric {}

// i*, u*, int, uint, c_*
@(lang(integral))
pub sealed trait Integral: Numeric {}

// i*, int, signed c_*
@(lang(signed))
pub sealed trait Signed: Integral {}

// i*, int, c_u*
@(lang(unsigned))
pub sealed trait Unsigned: Integral {}
