// Output: 1 1 2 2 3 3

fn main() {
    let a = [1, 2, 3][..];
    let b = [1, 2, 3][..];
    // TODO: fix type inference so this explicit specialization isnt necessary
    for (a, b) in a.iter().zip::<*int, std::span::Iter<int>>(b.iter()) {
        print("{a} {b} ");
    }
}
