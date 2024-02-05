#include <locale.h>

#ifndef CTL_NOGC
#include <gc.h>

#define CTL_MALLOC(sz)       GC_MALLOC(sz)
#define CTL_REALLOC(ptr, sz) GC_REALLOC(ptr, sz)
#else
#define CTL_MALLOC(sz)    malloc(sz)
#define CTL_REALLOC(a, b) realloc(a, b)
#endif

#define SINT(bits) _BitInt(bits)
#define UINT(bits) unsigned _BitInt(bits)

#ifdef NDEBUG
#define UNREACHABLE() __builtin_unreachable()
#else
#define UNREACHABLE()            \
    do {                         \
        __asm__ volatile("ud2"); \
        __builtin_unreachable(); \
    } while (0)
#endif

static inline void $ctl_init() {
#ifndef CTL_NOGC
    GC_INIT();
#endif
    setlocale(LC_ALL, "C.UTF-8");
}

typedef SINT(sizeof(void *) * 8) isize;
typedef UINT(sizeof(void *) * 8) usize;
typedef UINT(32) CTL_char;
typedef UINT(8) CTL_bool;
typedef float f32;
typedef double f64;
typedef struct {} $void;
