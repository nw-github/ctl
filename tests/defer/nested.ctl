// Output: 7 2 6 3 5 4 1

fn main() {
    defer print("1 ");
    defer {
        print("2 ");
        defer {
            print("3 ");
            defer print("4 ");
            print("5 ")
        }
        print("6 ");
    }
    print("7 ");
}
