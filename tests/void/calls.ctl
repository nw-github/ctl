// Output: hello
// Output: hello
// Output: hello

trait Foo {
    fn bar(this);
}

struct Bar {
    impl Foo {
        fn bar(this) { hello() }
    }
}

fn main() {
    let _ = hello();
    let _ = fnptr(hello);
    let _ = dynamic(&Bar());
}

fn hello() {
    println("hello");
}

fn fnptr(x: fn() => void) {
    x()
}

fn dynamic(f: *dyn Foo) {
    f.bar()
}
