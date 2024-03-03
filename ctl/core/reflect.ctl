// i*, u*, f32, f64, int, uint, c_*
#(lang(numeric))
pub trait Numeric {}

// i*, int, signed c_*
#(lang(signed))
pub trait Signed {}

// i*, int, c_u*
#(lang(unsigned))
pub trait Unsigned {}
