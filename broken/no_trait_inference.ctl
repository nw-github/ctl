use std::ops::Add;

fn main() {
    let lhs = [1, 2, 3][..];
    let _   = lhs.iter().zip(lhs.iter());

    let _   = Add::add(&1, &2);
}
