// Output: 2
// Output: 1
// Output: x: 10

fn main() {
    let x = @outer: {
        defer println("1");

        println("2");

        let _x = {
            break @outer 10;

            println("3");
        };

        println("4");
    };

    println("x: {x}");
}
