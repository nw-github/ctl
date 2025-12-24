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

@(lang(array))
pub sealed trait Array<T> {}

/// A unique numeric identifier for each type. This value is not stable and may change between
/// compilations of even identical source code, and thus should only be used for comparisons within
/// the same compilation unit and never serialized or read directly.
pub struct TypeId {
    tag: u64,

    pub fn ==(this, rhs: *This): bool => this.tag == rhs.tag;
    pub fn of<T>(): This => std::intrin::type_id::<T>();
    pub fn of_val<T>(_: *T): This => This::of::<T>();
}

pub use std::intrin::type_name;

pub fn type_name_of_val<T>(_: *T): str => type_name::<T>();
