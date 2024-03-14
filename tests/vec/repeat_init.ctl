// Output: 5
// Output: 255
// Output: 255
// Output: 255
// Output: 255
// Output: 255
// Output: true

fn main() {
    mut x = @[0xff; 5];
    println("{x.len()}");
    println("{x.pop()!}");
    println("{x.pop()!}");
    println("{x.pop()!}");
    println("{x.pop()!}");
    println("{x.pop()!}");
    println("{x.pop() is null}");
}