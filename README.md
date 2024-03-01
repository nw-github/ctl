# CTL

A general-purpose programming language that compiles to C.

## Example

```rust
trait Animal {
    fn make_sound(this, kw loud: bool);
}

struct Cat {
    pub name: str,

    impl Animal {
        fn make_sound(this, kw _loud: bool) {
            std::println("{this.name} said: purr...");
        }
    }
}

struct Dog {
    pub name: str,

    impl Animal {
        fn make_sound(this, kw loud: bool) {
            let msg = if loud { "WOOF! WOOF!" } else { "woof..." };
            std::println("{this.name} said: {msg}");
        }
    }
}

fn animal_sound<T: Animal>(animal: *T, kw loud: bool = false) {
    animal.make_sound(loud:);
}

fn main(): c_int {
    let cat = Cat(name: "Milo");
    let dog = Dog(name: "Rex");

    animal_sound(&cat);
    animal_sound(&dog, loud: false);
    animal_sound(&dog, loud: true);

    0
}

```

### Build
```
cargo r -r -- run main.ctl
```
