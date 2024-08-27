// Output: 2
// Output: 1
// Output: x: 10

fn main() {
    let x = @outer: {
        defer println("1");

        println("2");
        break @outer 10;
        println("3");
        5
    };

    println("x: {x}");
}
