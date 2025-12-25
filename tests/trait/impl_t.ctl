// Output: value: 3, 5

trait ReturnTuple<T, U> {
    fn get(this): (T, U);
}

trait Default {
    fn default(): This;
}

struct Foo {
    impl<T: Default, U: Default> ReturnTuple<T, U> {
        fn get(this): (T, U) => (T::default(), U::default());
    }
}

fn foo<T: ReturnTuple<int, uint>>(t: T) {
    let (a, b) = t.get();
    print("value: {a}, {b}");
}

extension Wow for int {
    impl Default {
        fn default(): int => 3;
    }
}

extension Wow2 for uint {
    impl Default {
        fn default(): uint => 5;
    }
}

fn main() {
    foo(Foo());
}
