// Error: type '[Foo; 4]' does not support subscript assign with arguments of type (int, Foo)
// Error: expression is not assignable
// Error: type '[Foo; 4]' does not support subscript assign with arguments of type (int, Foo)
// Error: expression is not assignable

struct Foo { x: int }

struct HasFoo {
    arr: [Foo; 4],

    fn a(self: HasFoo) {
        self.arr[0] = Foo(x: 10); // TODO: give this a better error
        self.arr[0].x = 10;
    }

    fn c(self: *HasFoo) {
        self.arr[0] = Foo(x: 10);
        self.arr[0].x = 10;
    }
}

fn main() { }
