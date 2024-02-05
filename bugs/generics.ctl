struct Hello<T, U> {
    fn something(): This {
        This() // should work
    }
}

// should say: expected 2 type params
fn foo(x: Hello) {}

fn main() {
    let a = Hello::something; // should say: needs type annotations
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
