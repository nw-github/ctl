// Error: expression is not assignable
// Error: expression is not assignable
// Error: expression is not assignable

struct Foo {
    pub x: i32,

    fn hello(mut this) {
        *this = Foo(x: 20);
        this = &mut Foo(x: 40);
    }
}

fn hello(x: *mut i32) {
    *x = 10;
    x = &mut 40;

    let z = &mut 10;
    *z = 20;
    z = &mut 30;
}

fn main() {}
