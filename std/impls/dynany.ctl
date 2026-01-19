use std::reflect::TypeId;

pub extension DynAnyImpl for *dyn std::any::Any {
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

pub extension DynMutAnyImpl for *dyn mut std::any::Any {
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
