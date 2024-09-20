use core::reflect::TypeId;

pub trait Any {
    fn type_id(this): TypeId;
}

pub mod ext {
    use super::TypeId;
    use super::*;

    pub extension AnyForAll<T> for T {
        impl Any {
            fn type_id(this): TypeId {
                TypeId::get::<T>()
            }
        }
    }
}
