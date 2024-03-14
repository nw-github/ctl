// Output: 3 4 2 5 1 3 2 1

fn main() {
    mut i = 0;
    while i < 2 {
        defer print("1 ");
        {
            defer print("2 ");
            print("3 ");
            if i == 1 {
                i++;
                continue;
            }

            print("4 ");
        }
        print("5 ");
        i++;
    }
}
