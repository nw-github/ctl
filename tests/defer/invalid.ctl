// Error: cannot return in defer block
// Error: break outside of loop

fn main() {
    defer {
        return 10;
    }

    loop {
        defer { break; }
    }
}
