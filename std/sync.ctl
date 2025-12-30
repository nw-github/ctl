pub use std::deps::libc::atomic::MemoryOrder;

use std::deps::libc::atomic::*;
use std::mem::Mutable;

pub struct Atomic<T> {
    val: Mutable<T>,

    pub fn new(val: T): This => Atomic(val: Mutable::new(val));

    @(inline(always))
    pub fn store(this, val: T, order: MemoryOrder = :SeqCst) {
        unsafe atomic_store_explicit(this.val.get(), val, order as u32);
    }

    @(inline(always))
    pub fn load(this, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_load_explicit(this.val.get(), order as u32)
    }

    @(inline(always))
    pub fn replace(this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_exchange_explicit(this.val.get(), val, order as u32)
    }

    // On success, this function returns null. On error, it returns Some(actual value)
    @(inline(always))
    pub fn compare_exchange(
        this,
        kw mut expected: T,
        kw val: T,
        kw success: MemoryOrder = :SeqCst,
        kw failure: MemoryOrder = :SeqCst,
    ): ?T {
        if !unsafe atomic_compare_exchange_strong_explicit(
            this.val.get(),
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
        this,
        kw mut expected: T,
        kw val: T,
        kw success: MemoryOrder = :SeqCst,
        kw failure: MemoryOrder = :SeqCst,
    ): ?T {
        if !unsafe atomic_compare_exchange_weak_explicit(
            this.val.get(),
            &mut expected,
            val,
            success as u32,
            failure as u32,
        ) {
            expected
        }
    }

    @(inline(always))
    pub fn is_lock_free(this): bool => unsafe atomic_is_lock_free(this.val.get());

    @(inline(always))
    pub fn as_raw(this): ^T => this.val.get();

    @(inline(always))
    pub fn as_raw_mut(this): ^mut T => this.val.get();

    @(inline(always))
    pub fn fetch_add(this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_fetch_add_explicit(this.val.get(), val, order as u32)
    }

    @(inline(always))
    pub fn fetch_sub(this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_fetch_sub_explicit(this.val.get(), val, order as u32)
    }

    @(inline(always))
    pub fn fetch_and(this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_fetch_and_explicit(this.val.get(), val, order as u32)
    }

    @(inline(always))
    pub fn fetch_or(this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_fetch_or_explicit(this.val.get(), val, order as u32)
    }

    @(inline(always))
    pub fn fetch_xor(this, val: T, order: MemoryOrder = :SeqCst): T {
        unsafe atomic_fetch_xor_explicit(this.val.get(), val, order as u32)
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
