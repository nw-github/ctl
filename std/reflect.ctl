// i*, u*, f32, f64, int, uint, c_*
@(lang(numeric))
pub sealed trait Numeric {}

// i*, u*, int, uint, c_*
@(lang(integral))
pub sealed trait Integral: Numeric {}

// i*, int, signed c_*
@(lang(signed))
pub sealed trait Signed: Integral {}

// u*, uint, c_u*
@(lang(unsigned))
pub sealed trait Unsigned: Integral {}

/// A unique numeric identifier for each type. This value is not stable and may change between
/// compilations of even identical source code, and thus should only be used for comparisons within
/// the same compilation unit and never serialized or read directly.
pub struct TypeId {
    tag: u64,

    pub fn ==(this, rhs: *TypeId): bool => this.tag == rhs.tag;
    pub fn get<T>(): TypeId => std::intrin::type_id::<T>();
}

pub use std::intrin::type_name;

pub fn type_name_of_val<T>(_: *T): str => type_name::<T>();
