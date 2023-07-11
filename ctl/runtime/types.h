#ifndef CTL_FWD_H 
#define CTL_FWD_H

#include <stdint.h>

#define CTL(ident)       CTL_##ident
#define CTL_BITINT(bits) _BitInt(bits)

typedef intptr_t  CTL(isize);
typedef uintptr_t CTL(usize);
typedef uint32_t  CTL(char);
typedef uint8_t   CTL(bool);

typedef float     CTL(f32);
typedef double    CTL(f64);

typedef struct {} CTL(void);

const CTL(bool) CTL(TRUE)  = 1;
const CTL(bool) CTL(FALSE) = 0;
const CTL(void) CTL(VOID)  = {};

#endif
