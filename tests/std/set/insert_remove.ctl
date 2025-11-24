// Output: 0
// Output: true true true true true 5
// Output: 0
// Output: true true true true true 5
// Output: true 4
// Output: false 4

fn main() {
    mut set: #[str] = #[];
    println("{set.len()}");

    let a = set.insert("jersey");
    let b = set.insert("intermediate");
    let c = set.insert("history");
    let d = set.insert("germany");
    let e = set.insert("farce");
    println("{a} {b} {c} {d} {e} {set.len()}");

    set.clear();
    println("{set.len()}");

    let a = set.insert("eternal");
    let b = set.insert("dragonfruit");
    let c = set.insert("cat");
    let d = set.insert("banana");
    let e = set.insert("apple");
    println("{a} {b} {c} {d} {e} {set.len()}");

    let a = set.remove(&"cat");
    println("{a} {set.len()}");

    let a = set.insert("apple");
    println("{a} {set.len()}");
}
