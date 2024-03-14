// Output: 0
// Output: true true true true true 5
// Output: 0
// Output: true true true true true 5
// Output: cat 4
// Output: apple 4

fn main() {
    mut map: [str: str] = [:];
    println("{map.len()}");

    let a = map.insert("e", "eternal");
    let b = map.insert("d", "dragonfruit");
    let c = map.insert("c", "cat");
    let d = map.insert("b", "banana");
    let e = map.insert("a", "apple");
    println("{a is null} {b is null} {c is null} {d is null} {e is null} {map.len()}");

    map.clear();
    println("{map.len()}");

    let a = map.insert("e", "eternal");
    let b = map.insert("d", "dragonfruit");
    let c = map.insert("c", "cat");
    let d = map.insert("b", "banana");
    let e = map.insert("a", "apple");
    println("{a is null} {b is null} {c is null} {d is null} {e is null} {map.len()}");

    let a = map.remove(&"c");
    println("{a!} {map.len()}");

    let a = map.insert("a", "asthma");
    println("{a!} {map.len()}");
}
