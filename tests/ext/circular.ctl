// Error: no method 'a' found on type 'int'
// Error: no method 'b' found on type 'int'
// Error: no method 'c' found on type 'int'

/*
trait Foo {}
trait Bar {}
trait Baz {}

extension<T: Baz> T {
    impl Foo {}

    fn a(this) {}
}

extension<T: Bar> T {
    impl Baz {}

    fn b(this) {}
}

extension<T: Foo> T {
    impl Bar {}

    fn c(this) {}
}

struct Hello {
    impl Foo {}
}

fn main() {
    let x = Hello();
    x.a();
    x.b();
    x.c();

    10.a();
    10.b();
    10.c();
}
 */