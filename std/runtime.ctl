use std::deps::libgc;

@(c_name("$ctl_stdlib_init"))
extern fn init() {
    @(feature(boehm))
    unsafe libgc::GC_init();
}

@(c_name("$ctl_stdlib_deinit"))
extern fn deinit() {
    @(feature(boehm))
    unsafe libgc::GC_deinit();
}
