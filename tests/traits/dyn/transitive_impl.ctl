// Output: 1
// Output: 0

fn main() {
    mut num = 0;
    let foo: *dyn Bar = &Test();
    foo.add(&mut num);
    println("{num}");

    foo.sub(&mut num);
    println("{num}");
}

pub trait Foo {
    fn add(this, x: *mut int);
}

pub trait Bar: Foo {
    fn sub(this, x: *mut int);
}

struct Test {
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
