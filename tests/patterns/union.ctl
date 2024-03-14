// Output: Bar::B(20)

union Bar {
    A,
    B(int),
}

fn main() {
    match Bar::B(20) {
        Bar::A => println("Bar::A"),
        Bar::B(val) => println("Bar::B({val})"),
    }
}
