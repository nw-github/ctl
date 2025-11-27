trait MakeInt {
    fn make(): int;
}

struct Foo<T: MakeInt> {
    bar: int = T::make(),
}

struct Test {
    impl MakeInt { fn make(): int { 1 } }
}

fn bar<T: MakeInt>(v: int = T::make()): int {
    v
}

fn main() {
    let x = Foo::<Test>();
    println("{x.bar}");

    let x = bar::<Test>();
    println("{x}")
}