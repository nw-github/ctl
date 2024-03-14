// Output: 3
// Output: 4
// Output: 3
// Output: 2
// Output: 4
// Output: 1
// Output: true

fn main() {
    mut x: [int] = @[];
    x.push(1);
    x.push(2);
    x.push(3);
    println("{x.len()}");

    x.insert(idx: 1, 4);
    println("{x.len()}");

    println("{x.pop()!}");
    println("{x.pop()!}");
    println("{x.pop()!}");
    println("{x.pop()!}");
    println("{x.pop() is null}");
}
