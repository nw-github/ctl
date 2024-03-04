// i*, u*, f32, f64, int, uint, c_*
#(lang(numeric))
pub trait Numeric {}

// i*, u*, int, uint, c_*
#(lang(integral))
pub trait Integral: Numeric {}

// i*, int, signed c_*
#(lang(signed))
pub trait Signed: Integral {}

// i*, int, c_u*
#(lang(unsigned))
pub trait Unsigned: Integral {}
