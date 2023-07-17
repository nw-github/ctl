#ifndef CTL_RUNTIME_H
#define CTL_RUNTIME_H

#include "types.h"

#include <gc.h>
#include <stdio.h>

inline CTL(void) print_i32(int32_t const num) {
    printf("%d", num);
    return CTL(VOID);
}

inline CTL(void) print_usize(CTL(usize) const num) {
    printf("%lu", num);
    return CTL(VOID);
}

inline CTL(void) print(uint8_t const *buf, CTL(usize) const len) {
    printf("%.*s", (int)len, buf);
    return CTL(VOID);
}

inline CTL(void) println(uint8_t const *buf, CTL(usize) const len) {
    printf("%.*s\n", (int)len, buf);
    return CTL(VOID);
}

inline CTL(void) eprint(uint8_t const *buf, CTL(usize) const len) {
    fprintf(stderr, "%.*s", (int)len, buf);
    return CTL(VOID);
}

inline CTL(void) eprintln(uint8_t const *buf, CTL(usize) const len) {
    fprintf(stderr, "%.*s\n", (int)len, buf);
    return CTL(VOID);
}

inline CTL(usize) ctl_malloc(CTL(usize) size) {
    return (CTL(usize))GC_MALLOC(size);
}

inline CTL(usize) ctl_realloc(uint8_t *old, CTL(usize) size) {
    return (CTL(usize))GC_REALLOC(old, size);
}

#endif // CTL_RUNTIME_H
