// Error: type mismatch: expected type 'fn() => void', found 'unsafe fn() => void'
// Error: type mismatch: expected type 'fn() => void', found 'extern "c" fn() => void'
// Error: type mismatch: expected type 'fn() => void', found 'extern "c" unsafe fn() => void'
// Error: type mismatch: expected type 'unsafe fn() => void', found 'extern "c" fn() => void'
// Error: type mismatch: expected type 'unsafe fn() => void', found 'extern "c" unsafe fn() => void'
// Error: type mismatch: expected type 'extern "c" fn() => void', found 'fn() => void'
// Error: type mismatch: expected type 'extern "c" fn() => void', found 'unsafe fn() => void'
// Error: type mismatch: expected type 'extern "c" fn() => void', found 'extern "c" unsafe fn() => void'
// Error: type mismatch: expected type 'extern "c" unsafe fn() => void', found 'fn() => void'
// Error: type mismatch: expected type 'extern "c" unsafe fn() => void', found 'unsafe fn() => void'
// Error: this operation is unsafe
// Error: this operation is unsafe

fn main() {
    mut sfn = &safe_fn;
    mut ufn = &unsafe_fn;

    mut sefn = &extern_fn;
    mut uefn = &extern_unsafe_fn;

    sfn = ufn;
    sfn = sefn;
    sfn = uefn;

    ufn = sfn;
    ufn = sefn;
    ufn = uefn;

    sefn = sfn;
    sefn = ufn;
    sefn = uefn;

    uefn = sfn;
    uefn = ufn;
    uefn = sefn;

    sfn();
    ufn();
    sefn();
    uefn();
}

fn safe_fn() {}
unsafe fn unsafe_fn() {}
extern fn extern_fn() {}
extern unsafe fn extern_unsafe_fn() {}
