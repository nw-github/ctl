// Output: 3

fn main() {
    let obj = NoOverride();
    mut val = 0;
    obj.add(&mut val);
    obj.add_twice(&mut val);
    println("{val}");
}

pub trait Foo {
    fn add(this, x: *mut int);

    fn add_twice(this, x: *mut int) {
        this.add(x);
        this.add(x);
    }
}

struct NoOverride {}

extension _ for NoOverride {
    impl Foo {
        fn add(this, x: *mut int) {
            *x += 1;
        }
    }
}
