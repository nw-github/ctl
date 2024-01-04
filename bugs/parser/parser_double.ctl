struct Foo<T: Bar<T>> {
    fn foo<U: Bar<U>>(u: Foo<Quux<U>>): Foo<Quux<U>> { u }
}

fn main() {
    let x = a.foo::<Foo<A>>();
    let y = &&x;
    let z = &&mut y;
}
