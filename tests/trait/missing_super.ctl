// Error: trait 'B' requires implementation of trait 'A'
// Error: trait 'D<int>' requires implementation of trait 'C<int>'
// Error: trait 'D<V>' requires implementation of trait 'C<V>'

trait A { }

trait B : A { }


trait C<T> {}

trait D<T>: C<T> {}


struct Hello<V> {
    impl B { }

    impl D<int> {}

    impl D<V> {}
}


fn main() {}
