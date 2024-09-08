// Error: cannot implement sealed trait 'Foo'

mod inner {
    pub sealed trait Foo {}

    pub struct Inner {
        bar: i32,

        impl Foo {}
    }
}

pub struct Outer {
    impl inner::Foo {}
}

fn main() {}
