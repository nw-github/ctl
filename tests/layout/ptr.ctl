// Output: true
// Output: true
// Output: true
// Output: true

// Output: true
// Output: true
// Output: true
// Output: true

fn main() {
    use std::mem::size_of;

    struct T {}

    println("{size_of::<*T>() == size_of::<int>()}");
    println("{size_of::<*T>() == size_of::<uint>()}");
    println("{size_of::<*T>() == size_of::<*mut T>()}");
    println("{size_of::<*T>() == size_of::<*raw T>()}");

    println("{size_of::<?*T>() == size_of::<*T>()}");
    println("{size_of::<?*mut T>() == size_of::<*T>()}");
    println("{size_of::<?*raw T>() == size_of::<*T>()}");
    println("{size_of::<?fn()>() == size_of::<*T>()}");
}
