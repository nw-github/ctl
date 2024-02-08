#define CTL_NOGC
#define CTL_NOBITINT
#include <locale.h>

#if !defined(CTL_NOGC)
#include <gc.h>

#define CTL_MALLOC(sz)       GC_MALLOC(sz)
#define CTL_REALLOC(ptr, sz) GC_REALLOC(ptr, sz)
#else
#include <stdlib.h>

#define CTL_MALLOC(sz)    malloc(sz)
#define CTL_REALLOC(a, b) realloc(a, b)
#endif

#if defined(CTL_NOBITINT)
#include <stdint.h>
#endif

#if defined(_MSC_VER)
#include <string.h>
#if defined(__cplusplus)
#define CTL_ZST [[msvc::no_unique_address]]
#else
#define CTL_ZST
#endif

#define CTL_DUMMY_INIT \
    { 0 }
#define CTL_DUMMY_MEMBER  CTL_ZST char dummy0[1]
#define CTL_NORETURN      __declspec(noreturn)
#define CTL_FORCEINLINE   __forceinline
#define CTL_MEMCPY        memcpy
#define CTL_MEMMOVE       memmove
#define CTL_MEMCMP        memcmp
#define CTL_STRLEN        strlen
#define CTL_UNREACHABLE() __assume(0)
#define CTL_ASSUME(x)     __assume(!!(x))

// courtesy of: https://stackoverflow.com/questions/1113409/attribute-constructor-equivalent-in-vc
// TODO: testing is required to see if or on what versions this gets optimized away
#pragma section(".CRT$XCU", read)
#define INIT_(f, p)                                          \
    static void f(void);                                     \
    __declspec(allocate(".CRT$XCU")) void (*f##_)(void) = f; \
    __pragma(comment(linker, "/include:" p #f "_")) static void f(void)
#if defined(_WIN64)
#define CTL_INIT(f) INIT_(f, "")
#else
#define CTL_INIT(f) INIT_(f, "_")
#endif

#define CTL_DEINIT(f) static void f(void)
#else
#define CTL_DUMMY_INIT
#define CTL_DUMMY_MEMBER
#define CTL_ZST
#define CTL_NORETURN    _Noreturn
#define CTL_FORCEINLINE __attribute__((always_inline))
#define CTL_MEMCPY      __builtin_memcpy
#define CTL_MEMMOVE     __builtin_memmove
#define CTL_MEMCMP      __builtin_memcmp
#define CTL_STRLEN      __builtin_strlen
#if defined(NDEBUG)
#define CTL_UNREACHABLE() __builtin_unreachable()
#else
#define CTL_UNREACHABLE()        \
    do {                         \
        __asm__ volatile("ud2"); \
        __builtin_unreachable(); \
    } while (0)
#endif
#define CTL_ASSUME(x)          \
    do {                       \
        if (!(x)) {            \
            CTL_UNREACHABLE(); \
        }                      \
    } while (0)

#define CTL_INIT(f)   static __attribute__((constructor)) void f(void)
#define CTL_DEINIT(f) static __attribute__((destructor)) void f(void)

#if defined(__clang__) && __clang_major__ < 14
#define _BitInt(x) _ExtInt(x)
#endif

#endif

#define SINT(bits) _BitInt(bits)
#define UINT(bits) unsigned _BitInt(bits)

const char *oldlocale;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wundefined-internal"
static void $ctl_static_init(void);
static void $ctl_static_deinit(void);
#pragma GCC diagnostic pop

CTL_DEINIT($ctl_runtime_deinit) {
    $ctl_static_deinit();

    setlocale(LC_ALL, oldlocale);
#if !defined(CTL_NOGC)
    // TODO: if linked as a dynamic library, unloaded, and reloaded, will this produce UB?
    GC_deinit();
#endif
}

CTL_INIT($ctl_runtime_init) {
#if !defined(CTL_NOGC)
    GC_INIT();
#endif
    oldlocale = setlocale(LC_ALL, "C.UTF-8");

    $ctl_static_init();

#if defined(_MSC_VER)
    int __cdecl atexit(void(__cdecl *)(void));

    atexit($ctl_runtime_deinit);
#endif
}

#if !defined(CTL_NOBITINT)
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
typedef struct {
    CTL_DUMMY_MEMBER;
} $void;

#define CTL_VOID       \
    ($void) {          \
        CTL_DUMMY_INIT \
    }
