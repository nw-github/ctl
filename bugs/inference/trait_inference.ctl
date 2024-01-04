pub extern fn printf(fmt: *c_char, ...): c_int;

pub struct Repeat<T> {
    elem: T,

    impl Iterator<T> {
        fn next(mut this): ?T {
            this.elem
        }
    }
}

pub struct Take<T, It: Iterator<T> > {
    iter: It,
    count: usize,

    impl Iterator<T> {
        fn next(mut this): ?T {
            if this.count > 0 {
                this.count--;
                this.iter.next()
            } else {
                null
            }
        }
    }
}

pub fn repeat<T>(elem: T): Repeat<T> {
    Repeat(elem:)
}

pub fn take<T, It: Iterator<T> >(iter: It, count: usize): Take<T, It> {
    Take(iter:, count:)
}

fn main() {
    // take should be able to infer T = i32, since It = Repeat<i32> which implements Iterator<i32>
    for elem in take(repeat(10), 5) {
        printf("%d\n".as_c_str(), elem);
    }
}
