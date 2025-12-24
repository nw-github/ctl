use std::reflect::TypeId;

pub trait Any {
    fn type_id(this): TypeId;
}

mod ext {
    use super::*;

    pub extension AnyForAll<T> for T {
        impl Any {
            fn type_id(this): TypeId => TypeId::of::<T>();
        }
    }
}
