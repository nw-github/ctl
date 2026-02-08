// Output: Cool
// Output: Some(10)
// Output: Some(5)
// Output: [1, 4, 5, 5, 7]
// Output: Foo

fn foo<T>(v: ?T): ?int {
    v.map(|_v| 10)
}

fn bar<U>(v: ?U) {
    let f = |=v| println("{v:?}");
    f();

    let f = |v: U| v;
    let f: fn(U) => U = f;
    f(v!);
}

trait Foo {
    fn foo();
}

fn baz<T: Foo>() {
    let v = || T::foo();
    v();
}

struct Epic<T> {
    fn cool<U>() {
        fn nice<V>() {
            let c = || println("Cool");
            c();
        }

        nice::<int>();
    }
}

extension int {
    impl Foo {
        fn foo() {
            println("Foo");
        }
    }
}

fn main() {
    Epic::<i32>::cool::<str>();

    let v = foo(?10);
    println("{v:?}");
    bar(?5);

    mut foo = [1, 5, 7, 4, 5];
    foo.sort_by_key(|c| *c);

    println("{foo:?}");

    baz::<int>();
}
