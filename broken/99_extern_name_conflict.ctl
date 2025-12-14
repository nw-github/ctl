extern fn test(): int {
    5
}

// should give an error
@(c_name(test))
extern fn other() {}

fn main() {
    let _ = test();
    other();
}
