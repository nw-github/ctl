// Output: 3
// Output: 10
// Output: true

fn main() {
    {
        mut x = @[1, 2, 3, 4, 5];
        let y = for i in x.iter_mut() {
            if *i == 3 {
                break i;
            }
        };

        let y = y!;
        println("{*y}");

        *y = 10;
        println("{*x.get(2)!}");
    }

    {
        mut x = @[1, 2, 3, 4, 5];
        let y = for i in x.iter() {
            if *i == 5000 {
                break *i;
            }
        };

        println("{y is null}");
    }
}
