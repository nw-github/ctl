#include <locale.h>

#ifdef CTL_NOBITINT
#include <stdint.h>
#endif

#ifndef CTL_NOGC
#include <gc.h>

#define CTL_MALLOC(sz)       GC_MALLOC(sz)
#define CTL_REALLOC(ptr, sz) GC_REALLOC(ptr, sz)
#else
#include <stdlib.h>

#define CTL_MALLOC(sz)    malloc(sz)
#define CTL_REALLOC(a, b) realloc(a, b)
#endif

#if defined(__clang__) && __clang_major__ < 15
#define _BitInt(x) _ExtInt(x)
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

#define CTL_INIT __attribute__((constructor))
#define CTL_DEINIT __attribute__((destructor))

const char *oldlocale;

static inline void $ctl_runtime_init() {
#ifndef CTL_NOGC
    GC_INIT();
#endif
    oldlocale = setlocale(LC_ALL, "C.UTF-8");
}

static inline void $ctl_runtime_deinit() {
    // TODO: if linked as a dynamic library, unloaded, and reloaded, will this produce UB?
#ifndef CTL_NOGC
    GC_deinit();
#endif
    setlocale(LC_ALL, oldlocale);
}

#ifndef CTL_NOBITINT
typedef SINT(sizeof(void *) * 8) isize;
typedef UINT(sizeof(void *) * 8) usize;
typedef UINT(32) CTL_char;
typedef UINT(8) CTL_bool;
#else
typedef intptr_t isize;
typedef uintptr_t usize;
typedef uint32_t CTL_char;
typedef uint8_t CTL_bool;
#endif

typedef float f32;
typedef double f64;
typedef struct {} $void;
