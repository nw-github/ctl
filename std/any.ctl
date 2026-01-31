use std::reflect::TypeId;

pub trait Any {
    fn type_id(this): TypeId;
}

$[lang(fallback_debug)]
extension<T> T {
    impl Any {
        fn type_id(this): TypeId => TypeId::of::<T>();
    }

    impl std::fmt::Debug {
        fn dbg(this, f: *mut std::fmt::Formatter) => std::intrin::builtin_dbg(this, f);
    }
}

extension *dyn std::any::Any {
    pub fn downcast<T>(my this): ?*T {
        if this.type_id() == TypeId::of::<T>() {
            unsafe this.downcast_unchecked()
        }
    }

    pub unsafe fn downcast_unchecked<T>(my this): *T {
        // TODO: maybe there should be a safer way to do this
        unsafe *(&raw this as ^*T)
    }
}

extension *dyn mut std::any::Any {
    pub fn downcast<T>(my this): ?*T {
        if this.type_id() == TypeId::of::<T>() {
            unsafe this.downcast_unchecked()
        }
    }

    pub fn downcast_mut<T>(my mut this): ?*mut T {
        if this.type_id() == TypeId::of::<T>() {
            unsafe this.downcast_unchecked_mut()
        }
    }

    pub unsafe fn downcast_unchecked<T>(my this): *T {
        unsafe *(&raw this as ^*T)
    }

    pub unsafe fn downcast_unchecked_mut<T>(my this): *mut T {
        unsafe *(&raw this as ^*mut T)
    }
}
