struct A {}

// doesn't complain A is not Hash + Eq<A>
fn foo(a: [A: u32]) {}

fn bar() {
    let x = [
        A(): 10,
        A(): 5,
    ];
}



trait Animal {}

struct Foo<T: Animal> {
    t: T,
}

// should say: i32 doesn't implement Animal
fn quux(foo: Foo<i32>) {}


