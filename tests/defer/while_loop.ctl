// Output: 3 2 3 2 4 1

fn main() {
    defer print("1");
    mut i = 0;
    while i++ < 2 {
        defer print("2 ");
        print("3 ");
    }
    print("4 ");
}
