// Output: 3
// Output: 4 Some(3) Some(2) Some(4) Some(1) null

fn main() {
    mut x: [int] = @[];
    x.push(1);
    x.push(2);
    x.push(3);
    println("{x.len()}");

    x.insert(idx: 1, 4);
    println("{x.len()} {x.pop():?} {x.pop():?} {x.pop():?} {x.pop():?} {x.pop():?}");
}
