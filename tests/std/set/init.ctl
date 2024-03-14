// Output: a b c

fn main() {
    let map = [
        10: "a",
        20: "b",
        30: "c",
    ];
    println("{*map.get(&10)!} {*map.get(&20)!} {*map.get(&30)!}");
}
