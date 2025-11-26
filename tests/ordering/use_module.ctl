// Output: pass

mod bar {
    pub use super::foo::inner::T;

    pub mod inner {
        pub union T { A, B }
    }
}

mod foo {
    pub use super::bar::inner;
}

// TODO: this should also work
// use T::A;

use bar::T;

fn main() {
    println("pass")
}
