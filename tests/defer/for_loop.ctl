// Output: 3 2 3 2 4 1

fn main() {
    defer print("1");
    for _i in 0..2 {
        defer print("2 ");
        print("3 ");
    }
    print("4 ");
}
