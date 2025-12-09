// Error: no symbol '_' found in this module

union Hi {
    A(i32),
    B(i32),
}

fn main() {
    let x = Hi::A(10);
    match x {
        :A(_) | :B(_) => {
            _;
        }
    }
}
