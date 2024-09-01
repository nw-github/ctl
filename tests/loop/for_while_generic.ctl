// Output: 1 2 3 1 2 3

fn test<T: std::fmt::Format>(t: [T..]) {
    for item in t.iter() {
        print("{item} ");
    }

    mut t = t.iter();
    while t.next() is ?item {
        print("{item} ");
    }
}

fn main() {
    test::<int>([1, 2, 3][..]);
}
