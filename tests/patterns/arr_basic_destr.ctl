// Output: 1 2 3 4
// Output: 1 2 3 4
// Output: 1 2 3 4
// Output: 1 2 3 4
// Output: 1 2 3 4
// Output: 1 2 3 4
// Output: 1 2 3 4
// Output: 1 2 3 4

fn main() {
    let funcs = @[
        &subscript, 
        val::end, 
        val::start, 
        val::mid,
        ptr::regular,
        ptr::end,
        ptr::start,
        ptr::mid,
    ];
    for func in funcs.iter() {
        let a = [1, 2, 3, 4];
        mut b = [5, 6, 7, 8];
        (*func)(a, &mut b);

        println("{b[0]} {b[1]} {b[2]} {b[3]}");
    }
}

pub fn subscript(a: [int; 4], b: *mut [int; 4]) {
    b[0] = a[0];
    b[1] = a[1];
    b[2] = a[2];
    b[3] = a[3];
}

mod val {
    pub fn end([x, y, ...end]: [int; 4], b: *mut [int; 4]) {
        b[0] = x;
        b[1] = y;
        b[2] = end[0];
        b[3] = end[1];
    }

    pub fn start([...start, x, y]: [int; 4], b: *mut [int; 4]) {
        b[0] = start[0];
        b[1] = start[1];
        b[2] = x;
        b[3] = y;
    }

    pub fn mid([x, ...mid, y]: [int; 4], b: *mut [int; 4]) {
        b[0] = x;
        b[1] = mid[0];
        b[2] = mid[1];
        b[3] = y;
    }
}

mod ptr {
    pub fn regular([a, b, c, d]: [int; 4], [x, y, z, w]: *mut [int; 4]) {
        *x = a;
        *y = b;
        *z = c;
        *w = d;
    }

    pub fn end([a, b, c, d]: [int; 4], [x, y, ...end]: *mut [int; 4]) {
        *x = a;
        *y = b;
        end[0] = c;
        end[1] = d;
    }

    pub fn start([a, b, c, d]: [int; 4], [...start, x, y]: *mut [int; 4]) {
        start[0] = a;
        start[1] = b;
        *x = c;
        *y = d;
    }

    pub fn mid([a, b, c, d]: [int; 4], [x, ...mid, y]: *mut [int; 4]) {
        *x = a;
        mid[0] = b;
        mid[1] = c;
        *y = d;
    }
}
