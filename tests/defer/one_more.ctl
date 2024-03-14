// Output: 3 2 1 2 1

fn main() {
    defer {
        mut i = 0;
        while i < 2 {
            defer print("1 ");
            print("2 ");
            i++;
        }
    }
    print("3 ");
}
