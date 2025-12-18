// Output: [2]
// Output: [2, 3]
// Output: [1, 2]
// Output: [1, 2, 3]
// Output: [1, 2, 3, 4]

fn main() {
    mut x = [1, 2, 3, 4];
    println("{x[1u..2u]:?}");
    println("{x[1u..=2u]:?}");
    println("{x[..2u]:?}");
    println("{x[..=2u]:?}");
    println("{x[..]:?}");
}
