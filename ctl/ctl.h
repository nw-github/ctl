#include <gc.h>
#include <stdint.h>
#include <stddef.h>
#include <locale.h>

#define SINT(bits) _BitInt(bits)
#define UINT(bits) unsigned _BitInt(bits)

#ifdef NDEBUG
#define UNREACHABLE() __builtin_unreachable()
#else
#define UNREACHABLE() do { __asm__ volatile("ud2"); __builtin_unreachable(); } while (0)
#endif

typedef SINT(sizeof(void *) * 8) isize;
typedef UINT(sizeof(void *) * 8) usize;
typedef UINT(32)                 CTL_char;
typedef UINT(8)                  CTL_bool;
typedef float                    f32;
typedef double                   f64;
