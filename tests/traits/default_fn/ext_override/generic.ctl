// Output: -1

fn main() {
    let obj = Override();
    mut val = 0;
    generic_call(&obj, &mut val);
    println("{val}");
}

fn generic_call<T: Foo>(obj: *T, val: *mut int) {
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

struct Override {}

extension _ for Override {
    impl Foo {
        fn add(this, x: *mut int) {
            *x += 1;
        }

        fn add_twice(this, x: *mut int) {
            *x -= 2;
        }
    }
}
