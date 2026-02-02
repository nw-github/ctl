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
        unsafe std::intrin::instance_ptr_of(this) as ^T as *T
    }
}

extension *dyn mut std::any::Any {
    pub fn downcast<T>(my this): ?*T => (this as *dyn Any).downcast();

    pub unsafe fn downcast_unchecked<T>(my this): *T {
        unsafe (this as *dyn Any).downcast_unchecked()
    }

    pub fn downcast_mut<T>(my this): ?*mut T {
        if this.type_id() == TypeId::of::<T>() {
            unsafe this.downcast_unchecked_mut()
        }
    }

    pub unsafe fn downcast_unchecked_mut<T>(my this): *mut T {
        unsafe std::intrin::instance_ptr_of(this) as ^mut T as *mut T
    }
}

$[cfg(test)]
mod tests {
    use super::*;

    unittest "downcast" {
        fn foo(a: *dyn Any): str {
            if a.downcast::<str>() is ?foo {
                "it was a string: {foo}".to_str()
            } else if a.downcast::<*int>() is ?foo {
                "it was an int ptr: {foo}".to_str()
            } else if a.downcast::<int>() is ?foo {
                "it was an int: {foo}".to_str()
            } else {
                "unknown type"
            }
        }

        assert_eq(foo(&"hello world"), "it was a string: hello world");
        assert_eq(foo(&&10), "it was an int ptr: 10");
        assert_eq(foo(&5), "it was an int: 5");
        assert_eq(foo(&&mut 10), "unknown type");
        assert_eq(foo(&[1, 2, 3]), "unknown type");
    }

    unittest "downcast_mut" {
        fn foo(a: *dyn mut Any): bool {
            if a.downcast_mut::<int>() is ?foo {
                *foo *= 3;
            } else if a.downcast_mut::<f64>() is ?foo {
                *foo += 1.0;
            } else {
                return false;
            }

            true
        }

        mut x = 10;
        assert(foo(&mut x));
        assert_eq(x, 30);

        mut x = 0.0;
        assert(foo(&mut x));
        assert_eq(x, 1.0);

        assert(!foo(&mut "hello"));
    }

    unittest "downcast from downcast_mut" {
        let ptr: *dyn mut Any = &mut 10;
        assert_eq(*ptr.downcast::<int>()!, 10);
    }
}
