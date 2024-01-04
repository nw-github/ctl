fn something_opt<T>(t: ?T) {}

fn main() {
    // should be able to infer T = i32, since T is implicitly convertible to ?T
    something_opt(10);
}