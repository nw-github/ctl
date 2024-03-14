// Output: 3 2 4 1

fn main() {
    defer print("1 ");
    {
        defer print("2 ");
        print("3 ");
    }
    print("4 ");
}
