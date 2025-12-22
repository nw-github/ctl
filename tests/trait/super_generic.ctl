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

trait A<T> : C<T> {
    fn foo(this): T;
}

trait B<T> : A<T> {
    fn bar(this): T;
    fn baz(this): T;
}

trait C<T> {
    fn quux(this): T;
}

struct Hello<V: std::ops::Add<V, V>> {
    a: V,
    b: V,
    c: V,

    impl A<V> {
        fn foo(this): V { this.a }
    }

    impl B<V> {
        fn bar(this): V { this.b }
        fn baz(this): V { this.c }
    }

    impl C<V> {
        fn quux(this): V { this.a + this.b + this.c }
    }
}

fn main() {
    let hi = Hello(a: 10, b: 5, c: 20);
    let a: *dyn A<int> = &hi;
    let b: *dyn B<int> = &hi;
    let c: *dyn C<int> = &hi;

    println("a.foo: {a.foo()}");
    println("a.quux: {a.quux()}");

    println("b.foo: {b.foo()}");
    println("b.bar: {b.bar()}");
    println("b.baz: {b.baz()}");
    println("b.quux: {b.quux()}");

    println("c.quux: {c.quux()}");

    let c_from_b: *dyn C<int> = b;
    println("c_from_b.quux: {c_from_b.quux()}");

    let a_from_b: *dyn A<int> = b;
    println("a_from_b.foo: {a_from_b.foo()}");
    println("a_from_b.quux: {a_from_b.quux()}");

    let c_from_a: *dyn C<int> = a;
    println("c_from_a.quux: {c_from_a.quux()}");
}
