// Output: 1 2 3 1 2 3

fn test<T: std::fmt::Format>(t: *mut [T]) {
    for item in t.iter_mut() {
        print("{item} ");
    }

    mut t = t.iter_mut();
    while t.next() is ?item {
        print("{item} ");
    }
}

fn main() {
    test::<int>(&mut @[1, 2, 3]);
}
