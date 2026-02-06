// Output: -1

fn main() {
    let obj = Override();
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

struct Override {}

extension Override {
    impl Foo {
        fn add(this, x: *mut int) {
            *x += 1;
        }

        fn add_twice(this, x: *mut int) {
            *x -= 2;
        }
    }
}
