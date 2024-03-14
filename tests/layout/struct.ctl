// Output: 0
// Output: 12
// Output: 12

fn main() {
    use std::mem::size_of;

    struct T {}

    struct MidPadding {
        x: i32,
        y: u8,
        z: i32,
    }

    union TailPadding {
        shared x: i32,

        Baz(u8),
        Quux(u16),
    }

    println("{size_of::<T>()}");
    println("{size_of::<MidPadding>()}");
    println("{size_of::<TailPadding>()}");
}
