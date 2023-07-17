#ifndef CTL_RUNTIME_H
#define CTL_RUNTIME_H

#include <gc.h>
#include <stdio.h>
#include <stdint.h>

#define CTL(ident)        CTL_##ident
#define CTL_SBITINT(bits) _BitInt(bits)
#define CTL_UBITINT(bits) unsigned _BitInt(bits)

typedef intptr_t    CTL(isize);
typedef uintptr_t   CTL(usize);
typedef uint32_t    CTL(char);
typedef uint8_t     CTL(bool);

typedef float       CTL(f32);
typedef double      CTL(f64);

inline void print_i32(int32_t const num) {
    printf("%d", num);
}

inline void print_usize(CTL(usize) const num) {
    printf("%lu", num);
}

inline void print(uint8_t const *buf, CTL(usize) const len) {
    printf("%.*s", (int)len, buf);
}

inline void println(uint8_t const *buf, CTL(usize) const len) {
    printf("%.*s\n", (int)len, buf);
}

inline void eprint(uint8_t const *buf, CTL(usize) const len) {
    fprintf(stderr, "%.*s", (int)len, buf);
}

inline void eprintln(uint8_t const *buf, CTL(usize) const len) {
    fprintf(stderr, "%.*s\n", (int)len, buf);
}

inline CTL(usize) ctl_malloc(CTL(usize) size) {
    return (CTL(usize))GC_MALLOC(size);
}

inline CTL(usize) ctl_realloc(uint8_t *old, CTL(usize) size) {
    return (CTL(usize))GC_REALLOC(old, size);
}

#endif // CTL_RUNTIME_H
