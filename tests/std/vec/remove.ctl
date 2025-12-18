// Output: 2
// Output: 6
// Output: 4
// Output: 5
// Output: [1, 7, 3, 5, 6]

fn main() {
    mut vec = @[1, 2, 3, 4, 5, 6, 7];

    println("{vec.swap_remove(1)}");
    println("{vec.len()}");
    println("{vec.remove(3)}");
    println("{vec.len()}");
    println("{vec[..]:?}");
}
