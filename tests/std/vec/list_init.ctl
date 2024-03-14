// Output: 3
// Output: 3
// Output: 2
// Output: 1
// Output: true

fn main() {
    mut x = @[1, 2, 3];
    println("{x.len()}");
    println("{x.pop()!}");
    println("{x.pop()!}");
    println("{x.pop()!}");
    println("{x.pop() is null}");
}

