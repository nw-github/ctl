#ifndef CTL_RUNTIME_H
#define CTL_RUNTIME_H

#include <gc.h>
#include <stdio.h>
#include <stdint.h>

#define CTL_SBITINT(bits) _BitInt(bits)
#define CTL_UBITINT(bits) unsigned _BitInt(bits)

typedef intptr_t    isize;
typedef uintptr_t   usize;
typedef uint32_t    CTL_char;
typedef uint8_t     CTL_bool;

typedef float       f32;
typedef double      f64;

inline void print_i32(int32_t const num) {
    printf("%d", num);
}

inline void print_usize(usize const num) {
    printf("%lu", num);
}

inline void print(uint8_t const *buf, usize const len) {
    printf("%.*s", (int)len, buf);
}

inline void println(uint8_t const *buf, usize const len) {
    printf("%.*s\n", (int)len, buf);
}

inline void eprint(uint8_t const *buf, usize const len) {
    fprintf(stderr, "%.*s", (int)len, buf);
}

inline void eprintln(uint8_t const *buf, usize const len) {
    fprintf(stderr, "%.*s\n", (int)len, buf);
}

inline usize ctl_malloc(usize size) {
    return (usize)GC_MALLOC(size);
}

inline usize ctl_realloc(uint8_t *old, usize size) {
    return (usize)GC_REALLOC(old, size);
}

#endif // CTL_RUNTIME_H
