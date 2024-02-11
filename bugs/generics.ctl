struct Hello<T, U> {
    fn something(): This {
        This() // should work
    }
}

struct A {}

// doesn't complain A is not Hash + Eq<A>
fn bar(a: [A: u32]) {}

trait Animal {}

struct Foo<T: Animal> {
    t: T,
}

// should say: i32 doesn't implement Animal
fn baz(foo: Foo<i32>) {}

fn quux<T>() {
    // should error out
    fn bar(t: T) {}
}
