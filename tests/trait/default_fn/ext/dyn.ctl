// Output: 3

fn main() {
    let obj = NoOverride();
    mut val = 0;
    call(&obj, &mut val);
    println("{val}");
}

fn call(obj: *dyn Foo, val: *mut int) {
    obj.add(val);
    obj.add_twice(val);
}

pub trait Foo {
    fn add(this, x: *mut int);

    fn add_twice(this, x: *mut int) {
        this.add(x);
        this.add(x);
    }
}

struct NoOverride {}

extension NoOverride {
    impl Foo {
        fn add(this, x: *mut int) {
            *x += 1;
        }
    }
}
