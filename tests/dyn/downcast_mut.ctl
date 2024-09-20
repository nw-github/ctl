// Output: 30
// Output: 1.0
// Output: unknown type

pub fn foo(a: *dyn mut std::any::Any) {
    if a.downcast_mut::<int>() is ?foo {
        *foo *= 3;
    } else if a.downcast_mut::<f64>() is ?foo {
        *foo += 1.0;
    } else {
        println("unknown type");
    }
}

fn main() {
    mut x = 10;
    foo(&mut x);
    println("{x}");

    mut x = 0.0;
    foo(&mut x);
    println("{x}");

    foo(&mut "hello");
}
