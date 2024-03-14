// Output: pass

fn main() {
    match "hello" {
        "goodbye" => println("hello matched 'goodbye'"),
        "oi" => println("hello matched 'oi'"),
        "bonjour" => println("hello matched 'bonjour'"),
        "hello" => println("pass"),
        _ => println("hello didn't match 'hello'"),
    }
}
