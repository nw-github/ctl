// Error: member 'd_bar' gives this struct infinite size
// Error: member 'd_foo' gives this struct infinite size
// Error: member 'fg_foo' gives this struct infinite size
// Error: member 's_foo' gives this struct infinite size
// Error: member 's_bar' gives this struct infinite size
// Error: member 'i_b' gives this struct infinite size
// Error: member 'i_c' gives this struct infinite size
// Error: member 'i_a' gives this struct infinite size
// Error: variant 'Union' gives this struct infinite size
// Error: member 'union_x' gives this struct infinite size
// Error: union tag makes this struct recursive
// Error: union tag must be an integer type

mod direct {
    struct Foo {
        d_bar: Bar,
    }

    struct Bar {
        d_foo: Foo,
    }
}

mod from_generic {
    struct Foo<T> {
        fg_t: T,
    }

    struct Bar {
        fg_foo: Foo<Bar>,
    }
}

mod self {
    struct Foo {
        s_foo: Foo,
    }

    struct Bar<T> {
        s_bar: Bar<T>,
    }
}

mod indirections {
    struct A { i_b: B, }
    struct B { i_c: C, }
    struct C { i_a: A, }
}

mod union_ {
    union Foo {
        Union(Foo),

        shared union_x: Foo,
    }

    union Bar: Bar { A, B, }
}
