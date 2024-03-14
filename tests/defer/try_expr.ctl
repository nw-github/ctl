// Output: 1 2 3

fn main() {
    test();
    print("3 ");
}

fn inner(): ?i32 {
    null
}

fn test(): ?i32 {
    defer print("2 ");

    print("1 ");
    let _value = inner()?;

    print("4 ");
    null
}
