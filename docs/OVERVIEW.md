# Introduction

This documentation is a work in progress and may be out of date. CTL is not stable and is evolving quickly, and features, functionality, and syntax may change without warning. Features marked "UNIMPLEMENTED" are planned but not currently implemented in the compiler.

# Table of contents

-   [Types](#types)
    -   [Integer Types](#integer-types)
    -   [Collection types](#collection-types)
    -   [Pointers](#pointers)
    -   [Operators](#operators)
-   [Control flow](#control-flow)
    -   [Basic control flow](#basic-control-flow)
    -   [Loops](#loops)
-   [Functions](#functions)
    -   [Closures](#closures)
-   [User types](#user-defined-types)
    -   [Structs](#structs)
    -   [Unions](#unions)
    -   [Unsafe unions](#unsafe-unions)
    -   [Methods](#methods)

# Types

## Integer Types

There are three kinds of integer types in CTL:

-   Regular integers: These integers start with `i` or `u` and are bit-precise, meaning you can write `i24`, `u38`, or any other number from `0 - 65535`. However, the types `u8`, `u16`, `u32`, `u64`, and `u128`, and their signed counterparts are the only ones that are FFI-safe.
-   Pointer sized integers: The types `int` and `uint`, which can be casted to and from raw pointers and are guaranteed to be pointer-sized, and the types `c_*`, which are equivalent to their C counterparts.
-   Integers by technicality: The types `bool` and `char` are technically integer types, even though they mostly behave uniquely. The only valid values for a `bool` are `true` and `false`. `char` is a [Unicode scalar value](https://www.unicode.org/glossary/#unicode_scalar_value). It's valid values are `0 - 0xD7FF` and `0xE000 - 0x10FFFF`. From now on, when integers/integer types are mentioned, these two types are NOT included.

All integers in CTL use 2s complement representation. Methods exist that can deal with wrapping safely (either by explicitly allowing it `wrapping_add()`, allowing it and telling you if it happened `overflowing_add()`, or returning an optional that is null on overflow `checked_add()`). The arithmetic operators panic by default on overflow, but this behavior is configurable by release mode (either panic or wrap on overflow, UNIMPLEMENTED).

Values of integer type are constructed from integer literals. Integer literals can implicitly convert to any integer type, and will default to type `int`. You can also apply an integer type suffix to a literal to get a literal of a specific integer type, and the special suffixes `u` and `i` create `uint` and `int` literals respectively.

```rs
let x = 10;     // Untyped literal, defaults to int
let y = 10u32;  // u32 typed literal
let z = 10u;    // uint typed literal
```

Integer types do not have any implicit conversion between each other, but there are several options for explicit conversions.

The `as` operator can only be used for integer casts which are infallible, meaning all values of the source type can be expressed in the target type. This means:

-   any `uX` to any `uY` where `Y >= X`
-   any `iX` to any `iY` where `Y >= X`
-   any `uX` to any `iY` where `Y >= X + 1`

For other casts, you will need a fallible cast operator. The `as!` operator will panic (UNIMPLEMENTED) if the conversion would overflow, and the `as?` (UNIMPLEMENTED) operator returns an optional that is `null` if the conversion failed, and `?value` if it didn't. Integer casts to/from `int`, `uint`, or any of the `c_*` types always require fallible casts, as their size may change from platform to platform.

There is also the standard library method `cast()`, available on all integer types, which performs a C-style cast with truncation/wrap-around.

## Collection types

The most basic collection type is the array `[T; N]`, a contiguous fixed-size collection of `N` `T`s, where `N` must be known at compile time. They can be created with an array literal, either `[expr; N]` (value, count), or `[A, B, C, ...]` (a list of values). Arrays are currently very limited, but they support a few operations. They can be indexed by any integer type, and a span can be created by indexing with a range type (ie `arr[0..x]`). Accessing an out-of-bounds index causes a panic (UNIMPLEMENTED).

```rs
let arr = [0; 10];     // create 10 0s on the stack
arr[0] = 1;
arr[1] = 2;
arr[2] = 3;
arr[3] = 4;
println("{arr[0] + arr[1] + arr[2] + arr[3]}"); // prints "10"
```

Byte string literals create a value of type `*[u8; N]`, where N is the length of the byte string.

```rs
let arr: [u8; 6] = *b"abcdef";
```

The next two types are the span types `[T..]` and `[mut T..]`. These are references to a contiguous collection of `T`s with runtime-known length. Slices can freely shrink, but can never grow (safely). `[mut T..]` allows modification of its elements, while `[T..]` only allows immutable access. They are otherwise identical, and instances of `[mut T..]` are implicitly convertible to `[T..]`.

```rs
let span = [1, 2, 3, 4][..];    // create span from temporary array
span[0] = 0;                    // mutable span, so modification is allowed
for i in span.iter() {          // iterate over the span
    print("{i} ");              // will print "0 2 3 4 "
}
```

There is also the type `str`, which acts very similar to (and is really just a thin wrapper around) `[u8..]`. String literals are guaranteed to be UTF8, and it is undefined behavior to create a `str` that contains non-UTF8 bytes. Slicing a string may panic, if the start or end index would be in the middle of a character. Indexing into a `str` returns `u8`s, but this usually isn't what you want. There are helper methods available to manipulate strings and iterate over `char`s instead of bytes.

```rs
let foo = "Tschüss!";
println("{foo[4]} {foo[4] as char}"); // prints "195 Ã"

for ch in foo.chars() {
    println("{ch as u32} {ch}");      // prints characters correctly
}

```

### Allocating collections

`[T]` is the vector type, a contiguous growable collection of `T`s. They can be created with a vector literal (`@[expr; N]` or `@[A, B, C, ...]`), and support indexing, slicing, and iteration.

```rs
mut foo = @[1, 2, 3];
foo.push(1);            // push 1 onto the back of the vector

let last = foo.pop();   // pop the last element from the vector.
println("{last}");      // prints "Some(1)"

foo.insert(idx: 0, 0);  // insert 0 at index 0
println("{foo[..]}");   // prints "[0, 1, 2, 3]"
```

`#[T]` is the set type, implemented with a hash set. Sets contain only unique elements and can be created with a set literal `#[A, B, C, ...]`. Sets cannot be indexed or sliced. They support iteration, but iteration order is unspecified. Sets can only be created from types that implement the `Hash` and `Eq` trait.

```rs
mut foo = #[1, 1, 2, 3, 4, 3];
foo.insert(2);
for i in foo.iter() {
    println("{i}");   // prints 1 2 3 4 once each
}

```

The final type is `[T: U]`, the map type. This is a dictionary of unique key-value pairs, implemented as a hash map. Like sets, there is syntax for map literals, iteration is supported but order is unspecified, slicing is unsupported, and the key type `T` must implement `Hash` and `Eq`. Unlike sets, the subscript operator is available. Assignment inserts a value into the map, and retrieval gets a value from the map, panicking if it doesn't exist.

```rs
mut bar = [10: "hello"];
bar[20] = "world";

let a = bar[&10]; // indexing by key requires a pointer
let b = bar[&20];
let c = bar[30, fallback: "!"]; // index with a fallback inserts the fallback into the map if the key doesn't exist
println("{a} {b}{c}"); // prints "hello world"
```

## Pointers

There are seven pointer types in CTL. All CTL pointer types are non-nullable, and creating a null pointer through another method (ex `std::mem::transmute`) is undefined behavior. Instead, use an optional pointer (like `?*T`/`?*mut T`), which is guaranteed to have the same layout as a normal pointer and is FFI-safe.

-   `*T` An pointer to an immutable `T`, created with `&expr`
-   `*mut T` A pointer to a mutable `T`, created with `&mut expr`

These are the standard pointer types. They must be non-null, aligned and point to a valid object.

-   `^T` A raw pointer to an immutable `T`. Creatable with `&raw expr`, implicitly convertible from `*T`/`*mut T`, and `as` castable to/from `*T`/`*mut T`
-   `^mut T` A raw pointer to a mutable `T`. Creatable with `&raw mut expr`, implicitly convertible from `*mut T`, and `as` castable to/from `*T`/`*mut T`

Raw pointers do not have the validity or alignment requirements of normal pointers, but they also must be non-null. Dereferencing or converting a raw pointer to a normal one requires an `unsafe` context.

-   `fn(A, B): R` A function pointer taking arguments of type A and B and returning R. Created from `&func`

Function pointers are not often needed due to dynamic dispatch, but they are occasionaly useful, especially for FFI. They do not currently support parameter labels.

-   `*dyn Trait` A dynamic immutable pointer to a type that implements `Trait`, implicitly convertible from a `*T`/`*mut T` where `T` implements `Trait`
-   `*dyn mut Trait` A dynamic mutable pointer to a type that implements `Trait`, implicitly convertible from a `*mut T` where `T` implements `Trait`

Dyn pointers are used for [dynamic dispatch](#traits), cannot be dereferenced, and are twice as wide as normal pointers.

Mutability of the pointer affects whether the pointed-to object can be mutated, not the pointer itself. Mutability is a feature of the pointer and not the base type, meaning there is no `mut T` type.

There are currently no additional constraints on CTL pointers (ie aliasing), but more may be added in the future as the memory model solidifies.

## Operators

The following is a table of operator availablilty on the built in types, and ability to override them for your own types.

### Binary operators

|     | Integer types | `^(mut) T` | `bool`    | Override Trait   |
| --- | ------------- | ---------- | --------- | ---------------- |
| +   | Same type     | Any int    | -         | std::ops::Add    |
| -   | Same type     | Any int    | -         | std::ops::Sub    |
| \*  | Same type     | -          | -         | std::ops::Mul    |
| /   | Same type     | -          | -         | std::ops::Div    |
| %   | Same type     | -          | -         | std::ops::Rem    |
| <<  | Any unsigned  | -          | -         | std::ops::Shl    |
| >>  | Any unsigned  | -          | -         | std::ops::Shr    |
| &   | Same type     | -          | Same type | std::ops::BitAnd |
| \|  | Same type     | -          | Same type | std::ops::BitOr  |
| ^   | Same type     | -          | Same type | std::ops::Xor    |
| and | -             | -          | Same type | -                |
| or  | -             | -          | Same type | -                |
| ==  | Same type     | Same type  | Same type | std::ops::Eq     |
| !=  | Same type     | Same type  | Same type | std::ops::Eq     |
| <=> | Same type     | Same type  | Same type | std::ops::Cmp\*  |

<sub>\* The comparison operator returns a `std::ops::Ordering` union with the variants `Less`, `Equal`, or `Greater`. This allows an implementation of one function to provide the operators `<`, `>`, `<=`, and `>=` automatically. In the future, an implementation of `Cmp` may also automatically provide an implementation of `Eq`, or at least the operators `==` and `!=`.</sub>

<sub>\*\* UNIMPLEMENTED: The compound assignment operators (`+=`, `-=`, etc.) are not currently overloadable.</sub>

### Unary operators

|     | `u*`/`uint`/`c_u*` | `i*`/`int`/`c_*` | `^(mut) T` | `bool` | Override Trait    |
| --- | ------------------ | ---------------- | ---------- | ------ | ----------------- |
| !\* | Yes                | Yes              | -          | Yes    | std::ops::Not     |
| -   | -                  | Yes              | -          | -      | std::ops::Neg     |
| ++  | Yes                | Yes              | Yes        | -      | std::ops::Inc\*\* |
| --  | Yes                | Yes              | Yes        | -      | std::ops::Dec\*\* |

<sub>\* ! performs a bitwise not. `bool` is essentially a `u1`, so bitwise not and logical not are equivalent.</sub>

<sub>\*\* The `Inc` and `Dec` traits cover both post and prefix operators (`x++` and `++x`). Postfix increment first copies the variable, then increments it by calling the trait method and returns the copy. Prefix increment first increments the variable, then returns a copy. In the future, this may force implementors of `Inc/Dec` to implement a `Clone` trait.</sub>

# Control flow

CTL supports a variety of control flow constructs, most of which are also expressions.

## Basic control flow

If expressions execute their body if the condition evaluates to true. Unlike C, the condition expression must be of type `bool` (no integers or pointers), curly braces are always required for the body, and parenthesis are not required for the condition. Any number of `else if`s may follow and zero or one `else` may terminate the expression.

```rs
let x = 10;
if x > 0 {
    println("{x} is greater than 0");
} else {
    println("things are broken!");
}
```

As the name suggests, `if` can be used as an expression, meaning it can yield a value. For example, an `if` used as a ternary operator:

```rs
let foo = if a > 10 { 2 } else { 5 };
```

There also exists an actual ternary operator, ... `then` ... `else` ..., which is just syntactic sugar for and has the exact same semantics as an if expression:

```rs
let foo = a > 10 then 2 else 5; // identical to the above code
```

If there is no `else` branch, the expression becomes optional, yielding `null` when the condition is false.

```rs
let a = 5;
let foo: ?int = if a > 10 { 2 };
println("{foo}"); // prints null
```

If the condition of an `if` expression includes an `is` expression, the variables bound by that expression are available in the body. This can be a convenient way to destructure one pattern without using a `match` expression.

```rs
let foo = Some(10);
if foo is ?inner {
    println("inner value: {inner}"); // inner is valid here!
}

if foo is ?inner and inner > 10 {
    println("bad! {inner}"); // inner is valid here, because an `and` was used, so inner would always be bound here
}

if !(foo is ?inner) {
    println("{inner} is not valid here!"); // error: no symbol 'inner' found in this module
}

```

If expressions are often used at the start of a function or loop to check that certain conditions are met, and return if they are not (often called a "guard clause" or "early return"). For convenience, this usage is canonicalized as a `guard` statement, which executes its body if the condition returns _false_.

```rs
fn add(a: uint, b: int): ?uint {
    guard b > 0 else {
        return null;
    }

    a + b as! uint
}

```

`guard` statements are more than just syntactic sugar. The body of a `guard` statement _must_ diverge (this is enforced at compile-time), and any variables created by pattern matching are available _after the body_, not inside it. This cannot be achieved with an `if` expression with an inverted condition.

```rs
let x: ?i32 = Some(5);
guard x is ?value and value < 10 else {
    panic("oh no!");
}

println("{value}"); // value can be used here

```

Match expressions are used to perform pattern matching. Match expressions must be exhaustive (have an arm that covers every case), which is checked at compile time.

```rs
union Foo {
    A(int),
    B(str),
    C
}

let foo = match Foo::C {
    Foo::A(1 | 6..10) => 1, // match any Foo::A where the inner value is 1 or 6 through 9 inclusive
    Foo::A(..-5) => 2,      // match any Foo::A where the inner value is less than -5
    Foo::A(15..) => 3,      // match any Foo::A where the inner value is greater than or equal to 15
    Foo::B("hello" | "world") => 4,
    Foo::C => 5,
    something_else => 6,
};

println("{foo}");

```

## Loops

There are four types of loop in CTL. The first type is the loop expression.

```rs
// Infinite unconditional loop
loop {}
```

All loops can yield a value by using the `break` keyword.

```rs
let x: int = loop {
    if y > z {
        break 10;
    }
};

```

While loops repeat their body as long as the condition is true. All properties of the `if` expression condition apply to the `while` loop.

```rs
mut (a, b) = (1, 10);
while a < b {
    println("{a}");
    a += 2;
}

mut foo = Some(5);
while foo is ?bar and bar < 10 {
    println("{bar}");
    foo = null;
}
```

While loops are also expressions, and can yield a value using the break keyword. The value will be optional, and `null` is returned if no break is executed or the loop never runs.

```rs
let foo = while false {
    break 10;
};

println("{foo}"); // prints null

```

Loop-while expressions are similar to while, but the condition is checked at the end, meaning they will always run at least once. Variables created by `is` expressions are not available in the body.

```rs
let a = 10;
let b = 15;
loop {
    println("{a} {b}"); // prints once
} while a > b;

```

Lastly, for expressions are the most complicated of the four. They can be used to iterate over anything that implements the `Iterator` trait, which most standard library containers have an adapter for.

```rs
for ch in "hello world!".chars() {
    println("{ch}");
}

// there is no c-style for loop, but ranges get the job done most of the time. other times, while loops + defer is preferred
for i in 0..10 {
    println("{i}");
}

// for is an expression, like while
let x = for foo in @[1, 2, 3].iter() {
    if foo > 10 {
        break *foo;
    }
};
```

# Functions

Functions are re-usable blocks of code that can take arguments and return a result.

```rs
fn hello(foo: int, bar: int): int {
    foo / bar
}

let res = hello(4, 2);
```

Like if and block expressions, a final expression without a semicolon returns that value to the caller. An explicit `return` expression can also be used. This example passed positional arguments to the function, but you can also use argument labels to pass them by the parameter names.

```rs
fn hello(foo: int, bar: int): int {
    foo / bar
}

let res = hello(bar: 2, foo: 4); // same result as the first example

```

When passing keyword arguments, the arguments are evaluated in left-to-right order as written, not by the declaration order of the parameters. A function can also force callers to pass keyword arguments with the `kw` keyword.

```rs
fn hello(kw foo: int, kw bar: int): int {
    foo / bar
}

let res = hello(4, 2); // this is now an error, foo and bar must be passed as keyword arguments.

```

Both argument passing styles can be mixed.

```rs
fn hello(foo: int, bar: int): int {
    foo / bar
}

let res = hello(bar: 2, 4); // same result as the first two examples

```

## Closures

UNIMPLEMENTED

# User defined types

Three different kinds of custom types can be created.

## Structs

```rs
struct Foo {
    a: u32,
    b: i64,
}
```

Structs can contain any number of member variables. All members must be initialized in the constructor (there is no concept of "zero" values in CTL), but you can give members a default expression which is cloned for every instantiation.

```rs
struct Bar {
    a: i32 = 10,
    b: i32 = foo(),
    c: i32,
}

let x = Bar(c: 10);         // a and b have default values and needn't be specified
let y = Bar(a: 5, c: 20);   // but they can be if desired

// foo is called twice here
```

The size of a struct can be obtained with the function `std::mem::size_of<T>()` (like any other type), but it will never be less than the combined size of all of its members. The representation of a struct in memory is unspecified, meaning the compiler may insert padding to align members or reorder members in memory as an optimization. (UNIMPLEMENTED: repr attribute for defined order)

This means that a struct like this:

```rs
struct Foo {
    x: u1,
    y: u3,
    z: u4,
}
```

will consume 3 bytes in memory, because each of the individual types has a size and alignment of 1 byte. If you have a struct of integers, or want to represent bitflags or bitfields conveniently, you can use a `packed struct`, which lays out its members with bit precision.

```rs
// This struct only takes up 1 byte!
packed struct Foo {
    x: u1,  // this member takes up bit 0
    y: u3,  // this member takes up bits 1..=3
    z: u4,  // this member takes up bits 4..=7
}

let foo = Foo(x: 1, y: 2, z: 3);
println("{foo.y}"); // prints 2

```

Packed structs are guaranteed to be layed out from top to bottom, least to most significant bit. They have identical syntax to normal structs for construction and member access, but behind the scenes, they use bit manipulation when storing or reading members. They also have a few restrictions that normal structs do not have. Firstly, you cannot take a direct pointer to a bitfield (a member of a packed struct) or call methods on bitfields that take the receiver by mutable reference. Currently, doing so results in a copy being created and a warning being issued, but it may become a hard error in the future.

```rs
packed struct Foo {
    x: u1,
    y: u3,
    z: u4,
}

let foo = Foo(x: 1, y: 2, z: 3);
let ptr = &foo.x; // this operation copies the member and creates a pointer to the temporary
```

Secondly, there are restrictions on the types that are allowed to be bitfields. Currently, only a few types are supported:

-   Integer types (`u*`, `i*`, `int`, `uint`)
-   `char`
-   `bool`
-   [Enum unions](#enum-unions)

In the future, more types may be supported, such as (raw) pointers, nested packed structs, or options.

## Unions

```rs
union Foo {
    A,
    B(i32, u32),
    C{x: i32, y: u32},
}
```

Union types may have any number of variants, of which only one can be active at a given time. Variants can be empty (have no data attached to them), tuple-like (have anonymous data attached to them), or struct-like (have named data attached to them). Pattern matching can be used to determine the active variant of a union and extract the data from variants.

```rs
let x = Foo::A;
match x {
    Foo::A => println("it was a!"),
    Foo::B(first, second) => println("it was b! first: {first}, second: {second}"),
    // struct like variant destructuring must match the name exactly. use the colon syntax to rename
    Foo::C{x, y: renamed} => println("it was c! x: {x}, y: {renamed}"),
}
```

If the union type can be easily inferred from context, the shorthand `:Variant` can be used.

```rs
extern fn bar(param: Foo);

bar(:A); // short for Foo::A

```

It is sometimes desirable to have a member that all union variants share. Instead of declaring the same type in each union variant, or wrapping a union in a struct, you can use a `shared` member.

```rs
union Bar {
    A,
    B(i32),

    shared x: i32,
}

mut foo = Bar::A(x: 10);
foo.x = 30; // x can be accessed directly, since all variants share it

```

Unions are implemented as tagged unions (AKA sum types, discriminated unions, etc.), a combination of an integer (the tag) to keep track of which variant is active, and a buffer to store the data (sized to the largest element). The tag type can be explicitly specified, otherwise the compiler will pick the smallest int needed.

```rs
union Foo { A, B, C } // tag type will be u2

union Bar: u64 { A, B, C } // tag type will be u64, even though only u2 is needed

```

The size of a union will be no less than the combined size of the tag and the size of the largest variant. Like with structs, the compiler may insert padding or reorder variant members, but because only one variant is active at a time, it is not necessary to have enough space for the combined size of all variant members. This offers an advantage over manually created tagged unions seen in languages that do not support unions or sum types:

```rs
// Size: 20 bytes
struct Foo {
    variant: u8,
    a: (i32, i32),
    b: (i32, i32),
}

// Size: 12 bytes
union Bar {
    A(i32, i32),
    B(i32, i32),
}
```

### Enum unions

Unions that contain no extra data in any variant are considered "enum unions" by the compiler. These types have the same layout and representation as their tag, can be infallibly casted to and fallibly casted from (UNIMPLEMENTED) integers, and can be packed struct members.

```rs
union Perms { R, W, X }

let x = Perms::R as u2;
let y = x as! Perms; // UNIMPLEMENTED: this doesn't actually work yet
```

## Unsafe unions

Unsafe unions are similar to regular unions when it comes to their size, but unlike regular unions, they do not automatically have a tag. There is no way to determine the active variant solely by inspecting the union, and reading any member other than the last one written to is undefined behavior. Due to this, reading a union member requires an unsafe context. Unsafe unions are created with a similar syntax to structs:

```rs
unsafe union Foo {
    a: i32,
    b: f32,
}

let x = Foo(a: 10);
unsafe {
    println("{x.a}");

    x.b = 5.0;
    println("{x.b}");
    println("{x.a}"); // UB in CTL, a was not the last member written to. use f64::to_bits() or std::mem::transmute() instead for this purpose
}
```

There is one additional difference between unsafe unions and structs. While struct construtors and union variant constructors can be used and passed around as functions, unsafe union constructors cannot. This is because unsafe union constructors are compiler magic, due to the need to take one of, but no combination of members.

```rs
struct Foo { x: i32, y: str }

unsafe union Bar { x: i32, y: str }

let x = &Foo;
let y = x(10, "hello");

let z = Bar; // this doesn't work
// let y = Bar(x: 5, y: "hello world"); // this generates a compile time error, but a "function" like this can't be represented in CTL
```

Unsafe unions mainly exist for FFI purposes and may be further restricted or removed in later versions. You should use `union`s instead.

## Methods

You can add methods to any user defined type by defining a function in its body.

```rs
struct Foo {
    a: int,
    b: str,

    fn display(this) {
        println("Foo, a: {this.a} b: {this.b}");
    }
}

```

Methods must define an explicit `this` parameter, which defaults to the type `*This`. There are two additional modifiers that modify the type of the `this` parameter.

-   `mut` makes the pointer mutable. `mut this` == `this: *mut This`
-   `my` makes the `this` parameter owned (will be copied/moved into the method). `my this` == `This`

`my` and `mut` can be combined, which will make the `this` binding mutable instead of affecting the type. If a function has no this parameter, it is treated as an associated function. These cannot be called on an instance of the type, but instead only through the namespace of the struct.

```rs
struct Foo {
    a: int,

    fn new(): Foo { Foo(a: 20) } // static method

    fn immutable(this) {
        println("{this.a}");
        // this.a = 0; // error
    }

    fn mutable(mut this) {
        this.a *= 2; // OK, changes are reflected in the caller
        // this = &mut *this; // BAD, this binding is immutable
    }

    fn owning(my this) {
        // this.a = 0; // error
        println("{this.a}");
        // currently this only affects how the arguments are passed, but move semantics will make
        // the `my` modifier more useful in the future
    }

    fn owning_mut(my mut this) {
        this.a = 0; // OK, changes are not reflected in the caller
    }
}

mut x = Foo::new(); // call static method through the struct namespace
x.mutable();
x.immutable();  // prints 20
x.owning_mut();
x.owning();     // prints 20 again
// x.new();     // error: new is a static method
```

Besides the additional syntax, methods are nothing special and behave like normal functions. This means you can take a reference to them and call them like any other function.

```rs
struct Foo {
    fn hello(this, a: i32) {}
}

let x = Foo();
let method: fn(*Foo, i32) = Foo::hello; // convert hello into a function pointer

method(&x, 10);
Foo::hello(&x, 10); // both work
```

# Generics

TODO

# Traits

TODO

# Extensions

TODO
