#include <gc.h>
#include <stdint.h>
#include <stddef.h>

#define CTL_SBITINT(bits) _BitInt(bits)
#define CTL_UBITINT(bits) unsigned _BitInt(bits)

typedef intptr_t  isize;
typedef uintptr_t usize;
typedef uint32_t  CTL_char;
typedef uint8_t   CTL_bool;
typedef float     f32;
typedef double    f64;

static inline void *ctl_malloc(usize size) {
    return GC_MALLOC(size);
}

static inline void *ctl_realloc(void *old, usize size) {
    return GC_REALLOC(old, size);
}
