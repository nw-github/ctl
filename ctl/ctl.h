#include <gc.h>
#include <stdint.h>
#include <stddef.h>
#include <locale.h>

#define SINT(bits) _BitInt(bits)
#define UINT(bits) unsigned _BitInt(bits)

typedef intptr_t  isize;
typedef uintptr_t usize;
typedef uint32_t  CTL_char;
typedef uint8_t   CTL_bool;
typedef float     f32;
typedef double    f64;
