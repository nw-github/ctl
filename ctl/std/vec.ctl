use core::mem::NonNull;
use core::option::Option;
use core::mem;
use core::panic;

pub struct Vec<T> {
    ptr: NonNull<T>,
    len: usize,
    cap: usize,

    pub fn new<U>() Vec<U> {
        return Vec::<U>(ptr: NonNull::dangling(), len: 0, cap: 0);
    }

    pub fn len(this) usize {
        return this.len;
    }

    pub fn capacity(this) usize {
        return this.cap;
    }

    pub fn push(mut this, t: T) {
        if !this.can_insert(1) {
            this.grow();
        }

        this.ptr.add(this.len++).write(t);
    }

    pub fn pop(mut this) ?T {
        return if this.len > 0 {
            yield this.ptr.add(--this.len).read();
        };
    }

    pub fn insert(mut this, idx: usize, t: T) {
        if idx > this.len {
            panic("Vec::insert(): index is greater than length!");
        }

        if !this.can_insert(1) {
            this.grow();
        }

        let src = this.ptr.add(idx);
        if idx < this.len {
            mem::move(
                dst: this.ptr.add(idx + 1).as_mut_ptr(), 
                src: src.as_ptr(), 
                num: this.len - idx
            );
        }

        src.write(t);
        this.len++;
    }

    pub fn reserve(mut this, cap: usize) {
        if cap <= this.cap {
            return;
        }

        match std::alloc::alloc::<T>(cap) {
            Option::Some(ptr) => {
                if this.len > 0 {
                    mem::copy(
                        dst: ptr.as_mut_ptr(), 
                        src: this.ptr.as_ptr(), 
                        num: this.len
                    );
                }

                this.ptr = ptr;
                this.cap = cap;
            },
            Option::None => panic("Out of memory!"),
        }
    }

    pub fn get(this, idx: usize) ?*T {
        return if idx < this.len {
            yield this.ptr.add(idx).as_ptr();
        };
    }

    pub fn get_mut(mut this, idx: usize) ?*mut T {
        return if idx < this.len {
            yield this.ptr.add(idx).as_mut_ptr();
        };
    }

    fn grow(mut this) {
        this.reserve(if this.cap > 0 {
            yield this.cap * 2;
        } else {
            yield 1;
        });
    }

    fn can_insert(this, count: usize) bool {
        return this.len + count <= this.cap;
    } 
}
