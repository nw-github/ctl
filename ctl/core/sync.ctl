pub union MemoryOrder {
    Relaxed,
    Consume,
    Acquire,
    Release,
    AcqRel,
    SeqCst,
}

mod c11 {
    #(c_opaque, c_name(ctl_atomic_store_explicit))
    pub extern fn atomic_store_explicit<T>(obj: *mut T, desired: T, order: u32);

    #(c_opaque, c_name(ctl_atomic_load_explicit))
    pub extern fn atomic_load_explicit<T>(obj: *T, order: u32): T;

    #(c_opaque, c_name(ctl_atomic_exchange_explicit))
    pub extern fn atomic_exchange_explicit<T>(obj: *mut T, desired: T, order: u32): T;

    #(c_opaque, c_name(ctl_atomic_compare_exchange_strong_explicit))
    pub extern fn atomic_compare_exchange_strong_explicit<T>(
        obj: *mut T,
        expected: *mut T,
        desired: T,
        success: u32,
        failure: u32,
    ): bool; // _Bool

    #(c_opaque, c_name(ctl_atomic_compare_exchange_weak_explicit))
    pub extern fn atomic_compare_exchange_weak_explicit<T>(
        obj: *mut T,
        expected: *mut T,
        desired: T,
        success: u32,
        failure: u32,
    ): bool; // _Bool

    #(c_opaque, c_name(ctl_atomic_fetch_add_explicit))
    pub extern fn atomic_fetch_add_explicit<T>(obj: *mut T, arg: T, order: u32): T;

    #(c_opaque, c_name(ctl_atomic_fetch_sub_explicit))
    pub extern fn atomic_fetch_sub_explicit<T>(obj: *mut T, arg: T, order: u32): T;

    #(c_opaque, c_name(ctl_atomic_fetch_and_explicit))
    pub extern fn atomic_fetch_and_explicit<T>(obj: *mut T, arg: T, order: u32): T;

    #(c_opaque, c_name(ctl_atomic_fetch_or_explicit))
    pub extern fn atomic_fetch_or_explicit<T>(obj: *mut T, arg: T, order: u32): T;

    #(c_opaque, c_name(ctl_atomic_fetch_xor_explicit))
    pub extern fn atomic_fetch_xor_explicit<T>(obj: *mut T, arg: T, order: u32): T;

    #(c_opaque, c_name(ctl_atomic_is_lock_free))
    pub extern fn atomic_is_lock_free<T>(obj: *T): bool; // _Bool
}

pub struct Atomic<T> {
    val: T,

    pub fn new(val: T): This {
        Atomic(val:)
    }

    #(inline(always))
    pub fn store(mut this, val: T, order: MemoryOrder = :SeqCst) {
        c11::atomic_store_explicit(&mut this.val, val, order as u32);
    }

    #(inline(always))
    pub fn load(this, order: MemoryOrder = :SeqCst): T {
        c11::atomic_load_explicit(&this.val, order as u32)
    }

    #(inline(always))
    pub fn replace(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        c11::atomic_exchange_explicit(&mut this.val, val, order as u32)
    }

    // On success, this function returns null. On error, it returns Some(actual value)
    #(inline(always))
    pub fn compare_exchange(
        mut this,
        kw mut expected: T,
        kw val: T,
        kw success: MemoryOrder = :SeqCst,
        kw failure: MemoryOrder = :SeqCst,
    ): ?T {
        if !c11::atomic_compare_exchange_strong_explicit(
            &mut this.val,
            &mut expected,
            val,
            success as u32,
            failure as u32,
        ) {
            expected
        }
    }

    #(inline(always))
    pub fn compare_exchange_weak(
        mut this,
        kw mut expected: T,
        kw val: T,
        kw success: MemoryOrder = :SeqCst,
        kw failure: MemoryOrder = :SeqCst,
    ): ?T {
        if !c11::atomic_compare_exchange_weak_explicit(
            &mut this.val,
            &mut expected,
            val,
            success as u32,
            failure as u32,
        ) {
            expected
        }
    }

    #(inline(always))
    pub fn is_lock_free(this): bool {
        c11::atomic_is_lock_free(&this.val)
    }

    #(inline(always))
    pub fn as_raw(this): *raw T {
        &raw this.val
    }

    #(inline(always))
    pub fn fetch_add(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        c11::atomic_fetch_add_explicit(&mut this.val, val, order as u32)
    }

    #(inline(always))
    pub fn fetch_sub(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        c11::atomic_fetch_sub_explicit(&mut this.val, val, order as u32)
    }

    #(inline(always))
    pub fn fetch_and(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        c11::atomic_fetch_and_explicit(&mut this.val, val, order as u32)
    }

    #(inline(always))
    pub fn fetch_or(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        c11::atomic_fetch_or_explicit(&mut this.val, val, order as u32)
    }

    #(inline(always))
    pub fn fetch_xor(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        c11::atomic_fetch_xor_explicit(&mut this.val, val, order as u32)
    }
}

// pub type AtomicU8 = Atomic<u8>;
// pub type AtomicU16 = Atomic<u16>;
// pub type AtomicU32 = Atomic<u32>;
// pub type AtomicU64 = Atomic<u64>;
// pub type AtomicU128 = Atomic<u128>;
// pub type AtomicUint = Atomic<uint>;

// pub type AtomicI8 = Atomic<i8>;
// pub type AtomicI16 = Atomic<i16>;
// pub type AtomicI32 = Atomic<i32>;
// pub type AtomicI64 = Atomic<i64>;
// pub type AtomicI128 = Atomic<i128>;
// pub type AtomicInt = Atomic<int>;
