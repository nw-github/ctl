use super::assert;

pub mod dd {
    use super::assert;

    trait Foo {
        fn foo(mut this, x: int);
    }

    pub fn normal() {
        struct Test {
            x: int,

            impl Foo {
                fn foo(mut this, x: int) {
                    this.x = x;
                }
            }
        }

        struct Test2 {
            x: int,

            impl Foo {
                fn foo(mut this, x: int) {
                    this.x = x + 5;
                }
            }
        }

        mut foo = Test(x: 0);
        mut bar = Test2(x: 0);

        mut x: *dyn mut Foo = &mut foo;
        x.foo(10);
        assert(foo.x == 10, "foo.x != 10");

        x = &mut bar;
        x.foo(10);
        assert(bar.x == 15, "bar.x != 15");
    }

    pub fn dependant() {
        trait Bar: Foo { }

        struct Test {
            x: int,
            
            impl Foo {
                fn foo(mut this, x: int) {
                    this.x = x;
                }
            }

            impl Bar {}
        }

        mut foo = Test(x: 0);
        mut x: *dyn mut Bar = &mut foo;
        x.foo(10);
        assert(foo.x == 10, "foo.x != 10");
    }

    pub fn recursive() {
        trait Bar: Baz {
            fn bar(mut this, x: int);
        }

        trait Baz: Bar {
            fn baz(mut this, x: int);
        }

        struct Test {
            x: int,

            impl Bar {
                fn bar(mut this, x: int) {
                    this.x += x;
                }
            }

            impl Baz {
                fn baz(mut this, x: int) {
                    this.x -= x;
                }
            }
        }

        mut foo = Test(x: 0);
        mut x: *dyn mut Bar = &mut foo;
        x.bar(10);
        assert(foo.x == 10, "foo.x != 10");
        x.baz(10);
        assert(foo.x == 0, "foo.x != 0");
    }
}

mod def_traits {
    pub trait Foo {
        fn add(this, x: *mut int);
    }

    pub trait Bar: Foo {
        fn sub(this, x: *mut int);

        fn add_twice(this, x: *mut int) {
            this.add(x);
            this.add(x);
        }
    }
}

pub mod default {
    use super::assert;
    use super::def_traits::*;

    struct NoOverride {
        impl Foo {
            fn add(this, x: *mut int) {
                *x += 1;
            }
        }

        impl Bar {
            fn sub(this, x: *mut int) {
                *x -= 1;
            }
        }
    }

    pub fn nooverride_direct() {
        mut y = 0;
        let x = &NoOverride();
        x.add(&mut y);
        assert(y == 1, "y != 1");

        x.sub(&mut y);
        assert(y == 0, "y != 0");

        x.add_twice(&mut y);
        assert(y == 2, "y != 2");
    }

    pub fn nooverride_dyn() {
        mut y = 0;
        let x: *dyn Bar = &NoOverride();
        x.add(&mut y);
        assert(y == 1, "y != 1");

        x.sub(&mut y);
        assert(y == 0, "y != 0");

        x.add_twice(&mut y);
        assert(y == 2, "y != 2");
    }

    pub fn nooverride_generic() {
        fn inner<T: Bar>(x: *T) {
            mut y = 0;
            x.add(&mut y);
            assert(y == 1, "y != 1");

            x.sub(&mut y);
            assert(y == 0, "y != 0");

            x.add_twice(&mut y);
            assert(y == 2, "y != 2");
        }

        inner(&NoOverride());
    }

    struct Override {
        impl Foo {
            fn add(this, x: *mut int) {
                *x += 1;
            }
        }

        impl Bar {
            fn sub(this, x: *mut int) {
                *x -= 1;
            }

            fn add_twice(this, x: *mut int) {
                this.sub(x);
                this.sub(x);
            }
        }
    }

    pub fn override_direct() {
        mut y = 0;
        let x = &Override();
        x.add(&mut y);
        assert(y == 1, "y != 1");

        x.sub(&mut y);
        assert(y == 0, "y != 0");

        x.add_twice(&mut y);
        assert(y == -2, "y != -2");
    }

    pub fn override_dyn() {
        mut y = 0;
        let x: *dyn Bar = &Override();
        x.add(&mut y);
        assert(y == 1, "y != 1");

        x.sub(&mut y);
        assert(y == 0, "y != 0");

        x.add_twice(&mut y);
        assert(y == -2, "y != -2");
    }

    pub fn override_generic() {
        fn inner<T: Bar>(x: *T) {
            mut y = 0;
            x.add(&mut y);
            assert(y == 1, "y != 1");

            x.sub(&mut y);
            assert(y == 0, "y != 0");

            x.add_twice(&mut y);
            assert(y == -2, "y != -2");
        }

        inner(&Override());
    }
}

pub mod default_ext {
    use super::assert;
    use super::def_traits::*;

    struct NoOverride {
        impl Foo {
            fn add(this, x: *mut int) {
                *x += 1;
            }
        }
    }

    extension NoOverrideExt for NoOverride {
        impl Bar {
            fn sub(this, x: *mut int) {
                *x -= 1;
            }
        }
    }

    pub fn nooverride_direct() {
        mut y = 0;
        let x = &NoOverride();
        x.add(&mut y);
        assert(y == 1, "y != 1");

        x.sub(&mut y);
        assert(y == 0, "y != 0");

        x.add_twice(&mut y);
        assert(y == 2, "y != 2");
    }

    pub fn nooverride_dyn() {
        mut y = 0;
        let x: *dyn Bar = &NoOverride();
        x.add(&mut y);
        assert(y == 1, "y != 1");

        x.sub(&mut y);
        assert(y == 0, "y != 0");

        x.add_twice(&mut y);
        assert(y == 2, "y != 2");
    }

    pub fn nooverride_generic() {
        fn inner<T: Bar>(x: *T) {
            mut y = 0;
            x.add(&mut y);
            assert(y == 1, "y != 1");

            x.sub(&mut y);
            assert(y == 0, "y != 0");

            x.add_twice(&mut y);
            assert(y == 2, "y != 2");
        }

        inner(&NoOverride());
    }

    struct Override {
        impl Foo {
            fn add(this, x: *mut int) {
                *x += 1;
            }
        }
    }

    extension OverrideExt for Override {
        impl Bar {
            fn sub(this, x: *mut int) {
                *x -= 1;
            }

            fn add_twice(this, x: *mut int) {
                this.sub(x);
                this.sub(x);
            }
        }
    }

    pub fn override_direct() {
        mut y = 0;
        let x = &Override();
        x.add(&mut y);
        assert(y == 1, "y != 1");

        x.sub(&mut y);
        assert(y == 0, "y != 0");

        x.add_twice(&mut y);
        assert(y == -2, "y != -2");
    }

    pub fn override_dyn() {
        mut y = 0;
        let x: *dyn Bar = &Override();
        x.add(&mut y);
        assert(y == 1, "y != 1");

        x.sub(&mut y);
        assert(y == 0, "y != 0");

        x.add_twice(&mut y);
        assert(y == -2, "y != -2");
    }

    pub fn override_generic() {
        fn inner<T: Bar>(x: *T) {
            mut y = 0;
            x.add(&mut y);
            assert(y == 1, "y != 1");

            x.sub(&mut y);
            assert(y == 0, "y != 0");

            x.add_twice(&mut y);
            assert(y == -2, "y != -2");
        }

        inner(&Override());
    }
}
