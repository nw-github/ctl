// Output: it was a string: hello world
// Output: it was an int ptr: 10
// Output: it was an int: 5

// Output: unknown type
// Output: unknown type

pub fn foo(a: *dyn std::any::Any) {
    if a.downcast::<str>() is ?foo {
        println("it was a string: {foo}");
    } else if a.downcast::<*int>() is ?foo {
        println("it was an int ptr: {foo}");
    } else if a.downcast::<int>() is ?foo {
        println("it was an int: {foo}");
    } else {
        println("unknown type");
    }
}

fn main() {
    foo(&"hello world");
    foo(&&10);
    foo(&5);

    foo(&&mut 10);
    foo(&[1, 2, 3]);
}
