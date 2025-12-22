// Output: a.foo: 10
// Output: a.quux: 35
// Output: b.foo: 10
// Output: b.bar: 5
// Output: b.baz: 20
// Output: b.quux: 35
// Output: c.quux: 35
// Output: c_from_b.quux: 35
// Output: a_from_b.foo: 10
// Output: a_from_b.quux: 35
// Output: c_from_a.quux: 35

trait A : C {
    fn foo(this): int;
}

trait B : A {
    fn bar(this): int;
    fn baz(this): int;
}

trait C {
    fn quux(this): int;
}

struct Hello {
    a: int,
    b: int,
    c: int,

    impl A {
        fn foo(this): int { this.a }
    }

    impl B {
        fn bar(this): int { this.b }
        fn baz(this): int { this.c }
    }

    impl C {
        fn quux(this): int { this.a + this.b + this.c }
    }
}

fn main() {
    let hi = Hello(a: 10, b: 5, c: 20);
    let a: *dyn A = &hi;
    let b: *dyn B = &hi;
    let c: *dyn C = &hi;

    println("a.foo: {a.foo()}");
    println("a.quux: {a.quux()}");

    println("b.foo: {b.foo()}");
    println("b.bar: {b.bar()}");
    println("b.baz: {b.baz()}");
    println("b.quux: {b.quux()}");

    println("c.quux: {c.quux()}");

    let c_from_b: *dyn C = b;
    println("c_from_b.quux: {c_from_b.quux()}");

    let a_from_b: *dyn A = b;
    println("a_from_b.foo: {a_from_b.foo()}");
    println("a_from_b.quux: {a_from_b.quux()}");

    let c_from_a: *dyn C = a;
    println("c_from_a.quux: {c_from_a.quux()}");
}
