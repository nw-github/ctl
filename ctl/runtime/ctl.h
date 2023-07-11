#ifndef CTL_RUNTIME_H
#define CTL_RUNTIME_H

#include "types.h"

#include <gc.h>
#include <stdio.h>

inline CTL(void) print_i32(int32_t const num) {
    printf("%d", num);
    return CTL(VOID);
}

inline CTL(void) print(uint8_t const *buf, CTL(usize) const len) {
    printf("%*s", (int)len, buf);
    return CTL(VOID);
}

inline CTL(void) println(uint8_t const *buf, CTL(usize) const len) {
    printf("%*s\n", (int)len, buf);
    return CTL(VOID);
}

inline CTL(void) eprint(uint8_t const *buf, CTL(usize) const len) {
    fprintf(stderr, "%*s", (int)len, buf);
    return CTL(VOID);
}

inline CTL(void) eprintln(uint8_t const *buf, CTL(usize) const len) {
    fprintf(stderr, "%*s\n", (int)len, buf);
    return CTL(VOID);
}

// inline CTL(char) char_at(CTL(str) const s, CTL(usize) pos) {    
//     return s.data[pos]; // TODO: UTF-8
// }
// 
// inline CTL(str) slice(CTL(str) const s, CTL(usize) start, CTL(usize) end) {    
//     return (CTL(str)){
//         .data = s.data + start,
//         .len  = end - start,
//     }; // TODO: UTF-8
// }

#endif // CTL_RUNTIME_H
