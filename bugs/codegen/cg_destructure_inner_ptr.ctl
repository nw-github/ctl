extern fn printf(fmt: *c_char, ...): c_int;

struct Vec2i {
    x: i32,
    y: i32,
}

union Foo {
    A(*mut Vec2i),
    B(*mut [i32; 2]),
}

fn by_val(x: Foo) {
    match x {
        Foo::A({x, y}) => {
            *x = 5;
            *y = 5;
        }
        Foo::B([x, y]) => {
            *x = 5;
            *y = 5;
        }
    }
}

fn by_ref(x: *mut Foo) {
    match x {
        Foo::A({x, y}) => {
            *x = 5;
            *y = 5;
        }
        Foo::B([x, y]) => {
            *x = 5;
            *y = 5;
        }
    }
}

fn main() {
    mut va = Vec2i(x: 10, y: 10);
    mut vb = Vec2i(x: 10, y: 10);
    mut aa = [10, 10];
    mut ab = [10, 10];

    // if matched_inner_type is a pointer, mark the destructure as a pointer
    by_val(Foo::A(&mut va));
    by_val(Foo::B(&mut aa));
    by_ref(&mut Foo::A(&mut vb));
    by_ref(&mut Foo::B(&mut ab));
}

// fn main(args: [str..]) {
//     let items = @[
//         Vec2i(x: 10, y: 15),
//         Vec2i(x: 7, y: 3),
//         Vec2i(x: 9, y: 14),
//         Vec2i(x: 3, y: 6463),
//     ];
// 
//     let p: ?*Vec2i = &Vec2i(x: 10, y: 20);
//     match p {
//         ?{x, y} => { }
//         _ => {}
//     }
// 
//     for {x, y} in items.iter() {
//         printf("%d, %d\n".as_c_str(), *x, *y);
//     }
// }
