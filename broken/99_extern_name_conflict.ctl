extern fn exit(code: c_int): never;

// crash at src/typecheck.rs:5473:22

// should give an error
@(c_name(exit))
extern fn exit2(code: c_int): void;
