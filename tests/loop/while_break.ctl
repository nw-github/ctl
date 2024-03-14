// Output: 5
// Output: true

fn main() {
    {
        mut x = 0;
        let y = while x < 10 {
            if x == 5 {
                break x;
            }
            x++;
        };
        println("{y!}");
    }

    {
        mut x = 0;
        let y = while x < 2 {
            if x == 5 {
                break x;
            }
            x++;
        };
        println("{y is null}");
    }
}