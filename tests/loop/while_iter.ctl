// Output: 10 true

fn main() {
    let x = @[1, 2, 3, 4];
    mut total = 0;
    mut iter = x.iter();
    while iter.next() is ?item {
        total += *item;
    }

    println("{total} {iter.next() is null}");
}
