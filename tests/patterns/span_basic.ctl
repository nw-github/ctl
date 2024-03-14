// Output: true
// Output: true
// Output: true
// Output: true
// Output: true

fn main() {
    mut vec = @[10, 10, 10, 10, 10];
    match vec.as_span_mut() {
        [a, b, ...mid, c, d] => {
            *a = 1;
            *b = 2;
            *mid.get_mut(0)! = 3;
            *c = 4;
            *d = 5;

            println("{*vec.get(0)! == 1}");
            println("{*vec.get(1)! == 2}");
            println("{*vec.get(2)! == 3}");
            println("{*vec.get(3)! == 4}");
            println("{*vec.get(4)! == 5}");
        }
        _ => {
            println("span pattern didnt match");
        }
    }
}
