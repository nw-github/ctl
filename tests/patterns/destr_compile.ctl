// Output: 1 2 3

struct Foo<T> {
    t: [T; 3],

    fn new<I: Iterator<T>>(mut t: I): ?Foo<T> {
        if [t.next()] is [?a] and (t.next(),) is (?b) and [t.next()][..] is [?c] {
            Foo(t: [a, b, *c])
        }
    }
}

fn main() {
    let foo = Foo::<*int>::new([1, 2, 3][..].iter())!;
    println("{foo.t[0]} {foo.t[1]} {foo.t[2]}");
}
