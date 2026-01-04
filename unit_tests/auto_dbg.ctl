packed struct PackedStruct { a: int, b: int }

unsafe union UnsafeUnion { a: u32, b: f32 }

union Union { Foo, Bar(u32), Baz(str, baz: int) }

struct Struct {
    int: int,
    tpl: (i32, other: u64),
    packed_struct: PackedStruct,
    unsafe_union: UnsafeUnion,
    normal_union: Union,
    opt_ptr: ?*uint,
    opt: ?str,
}

struct Pointers<T> {
    dynptr: *dyn std::any::Any,
    dynmutptr: *dyn mut std::any::Any,
    fnptr: fn(),
    fnobj: T,
}

extern fn sprintf(dst: ^mut c_char, fmt: ^c_char, ...): c_int;

fn addrs(p: *dyn std::any::Any): [^void; 2] => unsafe (&raw p).cast::<[^void; 2]>().read();

unittest "automatic debug impl" {
    let s = Struct(
        int: 10,
        tpl: (10, other: 25),
        packed_struct: PackedStruct(a: 5, b: 15),
        unsafe_union: UnsafeUnion(a: 10),
        normal_union: :Baz("hi", baz: 10),
        opt_ptr: ?&10,
        opt: "Hello world!",
    );

    let data = "Struct(int: 10, tpl: (10, other: 25), packed_struct: PackedStruct(a: 5, b: 15), unsafe_union: UnsafeUnion(a: 10, b: 1.4e-44), normal_union: Baz(\"hi\", baz: 10), opt_ptr: Some(10), opt: Some(\"Hello world!\"))";
    assert_eq("{s:?}".to_str(), data);

    fn cool() {}

    let ptrs = Pointers(dynptr: &10, dynmutptr: &mut 20, fnptr: &cool, fnobj: cool);
    let data = "{ptrs:?}".to_str();

    let templ = "Pointers(dynptr: *dyn Any \{self: %p, vtable: %p\}, dynmutptr: *dyn mut Any \{self: %p, vtable: %p\}, fnptr: %p, fnobj: %p)";

    let [self0, vtable0] = addrs(ptrs.dynptr);
    let [self1, vtable1] = addrs(ptrs.dynmutptr);

    mut buffer = [0u8; 2048];
    let len = unsafe sprintf(
        dst: buffer.as_raw_mut().cast(),
        templ.as_raw().cast(),
        self0,
        vtable0,
        self1,
        vtable1,
        ptrs.fnptr,
        ptrs.fnobj,
    ) as! uint;

    assert_eq(data, str::from_utf8(buffer[..len])!);
}
