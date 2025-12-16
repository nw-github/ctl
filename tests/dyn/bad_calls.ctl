// Error: cannot call generic functions through a dynamic pointer
// Error: associated function 'assoc' cannot be used as a method
// Error: cannot call method 'this_by_val' which takes this by value through a dynamic pointer
// Error: type mismatch: expected type 'This', found 'int'

trait Foo {
    fn hi(this);
    fn assoc(a: int);
    fn this_by_val(my this);
    fn generic<T>(this, v: *T);
    fn uses_this(this, v: This);
}

struct Bar {
    impl Foo {
        fn hi(this) { println("hi: {this as ^Bar:?}") }
        fn assoc(a: int) { println("assoc: {a}"); }
        fn this_by_val(my this) { println("this by val: {&this as ^Bar:?}"); }
        fn generic<T>(this, v: *T) { println("generic: {v as ^T:?}") }
        fn uses_this(this, v: Bar) { println("uses this: {&v as ^Bar:?}") }
    }
}

fn main() {
    let x: *dyn Foo = &Bar();
    x.hi();
    x.generic(&10);
    x.assoc();
    x.this_by_val();
    x.uses_this(10); // TODO: create an error for this explicitly
}

