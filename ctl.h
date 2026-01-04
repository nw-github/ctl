#if __STDC_VERSION__ >= 201112L
#  include <stdatomic.h>

// clang & msvc require that the pointers are to _Atomic types
#  if !defined(__GNUC__) || defined(__clang__)
#    define ctl_atomic_store_explicit(a, b, c) \
      atomic_store_explicit((_Atomic __typeof__(*(a)) *)a, b, c)
#    define ctl_atomic_load_explicit(a, b) \
      atomic_load_explicit((const _Atomic __typeof__(*(a)) *)a, b)
#    define ctl_atomic_exchange_explicit(a, b, c) \
      atomic_exchange_explicit((_Atomic __typeof__(*(a)) *)a, b, c)
#    define ctl_atomic_compare_exchange_strong_explicit(a, b, c, d, e) \
      atomic_compare_exchange_strong_explicit((_Atomic __typeof__(*(a)) *)a, b, c, d, e)
#    define ctl_atomic_compare_exchange_weak_explicit(a, b) \
      atomic_compare_exchange_weak_explicit((_Atomic __typeof__(*(a)) *)a, b, c, d, e)
#    define ctl_atomic_fetch_add_explicit(a, b, c) \
      atomic_fetch_add_explicit((_Atomic __typeof__(*(a)) *)a, b, c)
#    define ctl_atomic_fetch_sub_explicit(a, b, c) \
      atomic_fetch_sub_explicit((_Atomic __typeof__(*(a)) *)a, b, c)
#    define ctl_atomic_fetch_and_explicit(a, b, c) \
      atomic_fetch_and_explicit((_Atomic __typeof__(*(a)) *)a, b, c)
#    define ctl_atomic_fetch_or_explicit(a, b, c) \
      atomic_fetch_or_explicit((_Atomic __typeof__(*(a)) *)a, b, c)
#    define ctl_atomic_fetch_xor_explicit(a, b, c) \
      atomic_fetch_xor_explicit((_Atomic __typeof__(*(a)) *)a, b, c)
#    define ctl_atomic_is_lock_free(a) atomic_is_lock_free((_Atomic __typeof__(*(a)) *)a)
#  else
#    define ctl_atomic_store_explicit                   atomic_store_explicit
#    define ctl_atomic_load_explicit                    atomic_load_explicit
#    define ctl_atomic_exchange_explicit                atomic_exchange_explicit
#    define ctl_atomic_compare_exchange_strong_explicit atomic_compare_exchange_strong_explicit
#    define ctl_atomic_compare_exchange_weak_explicit   atomic_compare_exchange_weak_explicit
#    define ctl_atomic_fetch_add_explicit               atomic_fetch_add_explicit
#    define ctl_atomic_fetch_sub_explicit               atomic_fetch_sub_explicit
#    define ctl_atomic_fetch_and_explicit               atomic_fetch_and_explicit
#    define ctl_atomic_fetch_or_explicit                atomic_fetch_or_explicit
#    define ctl_atomic_fetch_xor_explicit               atomic_fetch_xor_explicit
#    define ctl_atomic_is_lock_free                     atomic_is_lock_free
#  endif
#endif

#include <stdint.h>

#if defined(_MSC_VER)
#  include <string.h>
#  if defined(__cplusplus)
#    define CTL_ZST [[msvc::no_unique_address]]
#  else
#    define CTL_ZST
#  endif

#  define CTL_NONNULL(...)
#  define CTL_DUMMY_INIT     0
#  define CTL_DUMMY_MEMBER   CTL_ZST char dummy
#  define CTL_NORETURN       __declspec(noreturn)
#  define CTL_FORCEINLINE    __forceinline inline
#  define CTL_NEVERINLINE    __declspec(noinline)
#  define CTL_INLINE         inline
#  define CTL_MEMCPY         memcpy
#  define CTL_MEMSET         memset
#  define CTL_MEMMOVE        memmove
#  define CTL_MEMCMP         memcmp
#  define CTL_STRLEN         strlen
#  define CTL_UNREACHABLE()  __assume(0)
#  define CTL_ASSUME(x)      __assume(!!(x))
#  define CTL_UNLIKELY(expr) (expr)
#  define CTL_LIKELY(expr)   (expr)

// courtesy of: https://stackoverflow.com/questions/1113409/attribute-constructor-equivalent-in-vc
// TODO: testing is required to see if or on what versions this gets optimized away
#  pragma section(".CRT$XCU", read)
#  define INIT_(f, p)                                        \
    static void f(void);                                     \
    __declspec(allocate(".CRT$XCU")) void (*f##_)(void) = f; \
    __pragma(comment(linker, "/include:" p #f "_")) static void f(void)
#  if defined(_WIN64)
#    define CTL_INIT(f) INIT_(f, "")
#  else
#    define CTL_INIT(f) INIT_(f, "_")
#  endif

#  define CTL_DEINIT(f) static void f(void)

#  define CTL_ALIGN(expr) __declspec(align(expr))
#else
#  define CTL_NONNULL(...) __attribute__((nonnull(__VA_ARGS__)))
#  define CTL_DUMMY_INIT
#  define CTL_DUMMY_MEMBER
#  define CTL_ZST
#  define CTL_NORETURN       _Noreturn
#  define CTL_FORCEINLINE    __attribute__((always_inline)) inline
#  define CTL_NEVERINLINE    __attribute__((noinline))
#  define CTL_INLINE         inline
#  define CTL_MEMCPY         __builtin_memcpy
#  define CTL_MEMSET         __builtin_memset
#  define CTL_MEMMOVE        __builtin_memmove
#  define CTL_MEMCMP         __builtin_memcmp
#  define CTL_STRLEN         __builtin_strlen
#  define CTL_UNLIKELY(expr) __builtin_expect(expr, 0)
#  define CTL_LIKELY(expr)   __builtin_expect(expr, 1)
#  if defined(__TINYC__)
#    define CTL_UNREACHABLE() __asm__ volatile("ud2")
#  else
#    if defined(NDEBUG)
#      define CTL_UNREACHABLE() __builtin_unreachable()
#    else
#      define CTL_UNREACHABLE() __builtin_trap()
#    endif
#  endif

#  define CTL_ASSUME(x)    \
    do {                   \
      if (!(x)) {          \
        CTL_UNREACHABLE(); \
      }                    \
    } while (0)

#  define CTL_INIT(f)   static __attribute__((constructor)) void f(void)
#  define CTL_DEINIT(f) static __attribute__((destructor)) void f(void)

#  define CTL_ALIGN(expr) __attribute__((aligned(expr)))

#  if defined(__clang__) && __clang_major__ < 14
#    define _BitInt(x) _ExtInt(x)
#  endif

#endif

#define COERCE(ty, expr) (expr, *(ty *)0)
#define VOID(expr)       (expr, CTL_VOID)

static void $ctl_static_init(void);
static void $ctl_static_deinit(void);

static void $ctl_stdlib_init(void);
static void $ctl_stdlib_deinit(void);

CTL_DEINIT($ctl_runtime_deinit) {
  $ctl_static_deinit();
  $ctl_stdlib_deinit();
}

CTL_INIT($ctl_runtime_init) {
  $ctl_static_init();
  $ctl_stdlib_init();

#if defined(_MSC_VER)
  int __cdecl atexit(void(__cdecl *)(void));

  atexit($ctl_runtime_deinit);
#endif
}

#ifdef __GNUC__
typedef __uint128_t uint128_t;
typedef __int128_t int128_t;
#endif

typedef intptr_t isize;
typedef uintptr_t usize;
typedef uint32_t $char;
typedef uint8_t $bool;

typedef float f32;
typedef double f64;
typedef struct {
  CTL_DUMMY_MEMBER;
} $void;

#define CTL_VOID   \
  ($void) {        \
    CTL_DUMMY_INIT \
  }

typedef void (*VirtualFn)(void);
typedef struct {
  void *self;
  VirtualFn const *vtable;
} DynMutPtr;

typedef struct {
  void const *self;
  VirtualFn const *vtable;
} DynPtr;

#ifdef CTL_HOSTED
static char **CTL_ARGV;
static int CTL_ARGC;
#endif

#ifdef __clang__
// These work at compile time, the type punning versions do not
// GCC has this function but only defines it in C++ for some reason
#  define CTL_IEEE_F64(v) __builtin_bit_cast(f64, (uint64_t)v)
#  define CTL_IEEE_F32(v) __builtin_bit_cast(f32, (uint32_t)v)
#else
typedef union {
  uint64_t in;
  f64 fp;
} $U64TOF64;

typedef union {
  uint32_t in;
  f32 fp;
} $U32TOF32;

#  define CTL_IEEE_F64(v) (($U64TOF64){.in = (uint64_t)v}).fp
#  define CTL_IEEE_F32(v) (($U32TOF32){.in = (uint32_t)v}).fp
#endif
