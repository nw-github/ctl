mod inner {
    pub struct A {}
}

mod inner {
    pub struct B {}
}

use inner::A;
use inner::B;

fn main() {
    let a = A();
    let b = B();
}
