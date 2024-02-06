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

#if defined(__clang__) && __clang_major__ < 14
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

// courtesy of: https://stackoverflow.com/questions/1113409/attribute-constructor-equivalent-in-vc
// TODO: testing is required to see if or on what versions this gets optimized away
#if defined(_MSC_VER)
#pragma section(".CRT$XCU", read)
#define INIT_(f, p)                                          \
    static void f(void);                                     \
    __declspec(allocate(".CRT$XCU")) void (*f##_)(void) = f; \
    __pragma(comment(linker, "/include:" p #f "_")) static void f(void)
#ifdef _WIN64
#define CTL_INIT(f) INIT_(f, "")
#else
#define CTL_INIT(f) INIT_(f, "_")
#endif

#define CTL_DEINIT(f) static void f(void)
#else
#define CTL_INIT(f)   static __attribute__((constructor)) void f(void)
#define CTL_DEINIT(f) static __attribute__((destructor)) void f(void)
#endif

const char *oldlocale;

static void $ctl_static_init(void);
static void $ctl_static_deinit(void);

CTL_DEINIT($ctl_runtime_deinit) {
    $ctl_static_deinit();

    setlocale(LC_ALL, oldlocale);
#ifndef CTL_NOGC
    // TODO: if linked as a dynamic library, unloaded, and reloaded, will this produce UB?
    GC_deinit();
#endif
}

CTL_INIT($ctl_runtime_init) {
#ifndef CTL_NOGC
    GC_INIT();
#endif
    oldlocale = setlocale(LC_ALL, "C.UTF-8");

    $ctl_static_init();

#if defined(_MSC_VER)
    int __cdecl atexit(void(__cdecl *)(void));

    atexit($ctl_runtime_deinit);
#endif
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

#if !defined(_MSC_VER)
typedef struct {
} $void;

#define CTL_VOID \
    ($void) { }
#else
typedef struct {
    char _[0];
} $void;

#define CTL_VOID \
    ($void) {    \
        { 0 }    \
    }
#endif
