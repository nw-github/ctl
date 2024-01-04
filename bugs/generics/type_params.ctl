struct Hello<T, U> {}

// should say: expected 2 type params
fn foo(x: Hello) {}

trait Animal {}

struct Foo<T: Animal> {
    t: T,
}

// should say: i32 doesn't implement Animal
fn bar(foo: Foo<i32>) {}

