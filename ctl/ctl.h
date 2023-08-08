#include <gc.h>
#include <stdint.h>
#include <stddef.h>
#include <locale.h>

#define SINT(bits) _BitInt(bits)
#define UINT(bits) unsigned _BitInt(bits)
#define UNREACHABLE() __asm__ volatile("ud2")

typedef SINT(sizeof(void *) * 8) isize;
typedef UINT(sizeof(void *) * 8) usize;
typedef UINT(32)                 CTL_char;
typedef UINT(8)                  CTL_bool;
typedef float                    f32;
typedef double                   f64;
