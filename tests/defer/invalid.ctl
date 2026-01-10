// Error: cannot return in defer block
// Error: break outside of loop
// Error: operator '?' cannot be used in a defer block

fn other(): ?int {
    defer {
        let v = ?10;
        v?;
    }

    10
}

fn main() {
    defer {
        return 10;
    }

    loop {
        defer { break; }
    }
}
