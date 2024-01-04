struct A { foo: i32, }

// says redeclaration of function A, should say redeclaration of struct A
struct A { foo: f32, }



union B { Foo(i32), Bar(u32), }

// should error
union B { Foo, Bar, }




use core::ptr::Raw;

// should error
struct Raw {}
