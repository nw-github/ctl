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

#define CTL_NONNULL()
#define CTL_DUMMY_INIT    0
#define CTL_DUMMY_MEMBER  CTL_ZST char dummy
#define CTL_NORETURN      __declspec(noreturn)
#define CTL_FORCEINLINE   __forceinline
#define CTL_MEMCPY        memcpy
#define CTL_MEMSET        memset
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
#define CTL_NONNULL(...) __attribute__((nonnull(__VA_ARGS__)))
#define CTL_DUMMY_INIT
#define CTL_DUMMY_MEMBER
#define CTL_ZST
#define CTL_NORETURN    _Noreturn
#define CTL_FORCEINLINE __attribute__((always_inline))
#define CTL_MEMCPY      __builtin_memcpy
#define CTL_MEMSET      __builtin_memset
#define CTL_MEMMOVE     __builtin_memmove
#define CTL_MEMCMP      __builtin_memcmp
#define CTL_STRLEN      __builtin_strlen
#if !defined(__TINYC__)
#if defined(NDEBUG)
#define CTL_UNREACHABLE() __builtin_unreachable()
#else
#define CTL_UNREACHABLE()        \
    do {                         \
        __asm__ volatile("ud2"); \
        __builtin_unreachable(); \
    } while (0)
#endif
#else
#define CTL_UNREACHABLE() __asm__ volatile("ud2")
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
#define STRLIT(s, data, n)                               \
    (s) {                                                \
        .$span = {.$ptr = (u8 *)data, .$len = (usize)n } \
    }

#pragma clang diagnostic ignored "-Wundefined-internal"
static void $ctl_static_init(void);
static void $ctl_static_deinit(void);

CTL_DEINIT($ctl_runtime_deinit) {
    $ctl_static_deinit();

#if !defined(CTL_NOGC)
    GC_deinit();
#endif
}

CTL_INIT($ctl_runtime_init) {
#if !defined(CTL_NOGC)
    GC_INIT();
#endif
    $ctl_static_init();

#if defined(_MSC_VER)
    int __cdecl atexit(void(__cdecl *)(void));

    atexit($ctl_runtime_deinit);
#endif
}

#if !defined(CTL_NOBITINT)
typedef SINT(sizeof(void *) * 8) isize;
typedef UINT(sizeof(void *) * 8) usize;
typedef UINT(32) $char;
typedef UINT(8) $bool;
#else
typedef intptr_t isize;
typedef uintptr_t usize;
typedef uint32_t $char;
typedef uint8_t $bool;
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
