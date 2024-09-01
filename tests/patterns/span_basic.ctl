// Output: true true true true true

fn main() {
    mut vec = [10, 10, 10, 10, 10][..];
    match vec {
        [a, b, ...mid, c, d] => {
            *a = 1;
            *b = 2;
            *mid.get_mut(0)! = 3;
            *c = 4;
            *d = 5;

            print("{*vec.get(0)! == 1} ");
            print("{*vec.get(1)! == 2} ");
            print("{*vec.get(2)! == 3} ");
            print("{*vec.get(3)! == 4} ");
            print("{*vec.get(4)! == 5} ");
        }
        _ => {
            println("span pattern didnt match");
        }
    }
}
