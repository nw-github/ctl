// Output: 3 Some(3) Some(2) Some(1) null

fn main() {
    mut x = @[1, 2, 3];
    print("{x.len()} {x.pop():?} {x.pop():?} {x.pop():?} {x.pop():?}");
}

