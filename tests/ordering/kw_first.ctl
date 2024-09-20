// Output: hello 10

fn regular(a: *str, b: i32) { println("{a} {b}") }

fn main() {
    regular(a: &"hello", 10);
}
