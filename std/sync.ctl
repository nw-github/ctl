pub use std::libc::atomic::MemoryOrder;

use std::libc::atomic::*;

pub struct Atomic<T> {
    val: T,

    pub fn new(val: T): This {
        Atomic(val:)
    }

    @(inline(always))
    pub fn store(mut this, val: T, order: MemoryOrder = :SeqCst) {
        unsafe atomic_store_explicit(&mut this.val, val, order as u32);
    }

    @(inline(always))
    pub fn load(this, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_load_explicit(&this.val, order as u32)
    }

    @(inline(always))
    pub fn replace(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_exchange_explicit(&mut this.val, val, order as u32)
    }

    // On success, this function returns null. On error, it returns Some(actual value)
    @(inline(always))
    pub fn compare_exchange(
        mut this,
        kw mut expected: T,
        kw val: T,
        kw success: MemoryOrder = :SeqCst,
        kw failure: MemoryOrder = :SeqCst,
    ): ?T {
        if !unsafe atomic_compare_exchange_strong_explicit(
            &mut this.val,
            &mut expected,
            val,
            success as u32,
            failure as u32,
        ) {
            expected
        }
    }

    @(inline(always))
    pub fn compare_exchange_weak(
        mut this,
        kw mut expected: T,
        kw val: T,
        kw success: MemoryOrder = :SeqCst,
        kw failure: MemoryOrder = :SeqCst,
    ): ?T {
        if !unsafe atomic_compare_exchange_weak_explicit(
            &mut this.val,
            &mut expected,
            val,
            success as u32,
            failure as u32,
        ) {
            expected
        }
    }

    @(inline(always))
    pub fn is_lock_free(this): bool => unsafe atomic_is_lock_free(&this.val);

    @(inline(always))
    pub fn as_raw(this): ^T => &raw this.val;

    @(inline(always))
    pub fn as_raw_mut(mut this): ^mut T => &raw mut this.val;

    @(inline(always))
    pub fn fetch_add(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_fetch_add_explicit(&mut this.val, val, order as u32)
    }

    @(inline(always))
    pub fn fetch_sub(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_fetch_sub_explicit(&mut this.val, val, order as u32)
    }

    @(inline(always))
    pub fn fetch_and(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_fetch_and_explicit(&mut this.val, val, order as u32)
    }

    @(inline(always))
    pub fn fetch_or(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_fetch_or_explicit(&mut this.val, val, order as u32)
    }

    @(inline(always))
    pub fn fetch_xor(mut this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_fetch_xor_explicit(&mut this.val, val, order as u32)
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
