extern fn exit(code: c_int): never;

// should give an error
[c_name(exit)]
extern fn exit2(code: c_int): void;


