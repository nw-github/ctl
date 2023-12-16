# CTL

A general-purpose programming language that compiles to C.

## Example

```rust
extern fn printf(fmt: *c_char, ...) c_int;

trait Animal {
    fn make_sound(this, kw loud: bool);
}

struct Cat: Animal {
    pub name: str,

    fn make_sound(this, kw loud: bool) {
        printf("%s said: purr...\n".as_c_str(), this.name.as_c_str());
    }
}

struct Dog: Animal {
    pub name: str,

    fn make_sound(this, kw loud: bool) {
        printf(
            "%s said: %s\n".as_c_str(), 
            this.name.as_c_str(), 
            if loud { yield "WOOF! WOOF!"; } else { yield "woof..."; }.as_c_str(),
        );
    }
}

fn animal_sound<T: Animal>(animal: *T, kw loud: bool = false) {
    animal.make_sound(loud:);
}

fn main() c_int {
    let cat = Cat(name: "Milo");
    let dog = Dog(name: "Rex");

    animal_sound(&cat);
    animal_sound(&dog, loud: false);
    animal_sound(&dog, loud: true);

    return 0;
}

```

### Build
```
cargo r -r -- main.ctl > main.c
clang -std=c11 -lgc -lm main.c
./a.out
```
