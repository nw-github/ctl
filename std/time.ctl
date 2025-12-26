use std::libc;

const NS_PER_SEC: u64 = 1_000_000_000;

pub struct Duration {
    secs: u64,
    nanos: u32,

    pub fn new(kw mut secs: u64, kw mut nanos: u32): This {
        secs += (nanos / NS_PER_SEC as! u32) as u64;
        nanos %= NS_PER_SEC as! u32;
        Duration(secs:, nanos:)
    }
    pub fn zero(): This => Duration(secs: 0, nanos: 0);

    pub fn from_nanos(ns: u64): This => Duration(secs: ns / NS_PER_SEC, nanos: (ns % NS_PER_SEC) as! u32);
    pub fn from_micros(us: u64): This => This::from_nanos(us * 1_000);
    pub fn from_millis(ms: u64): This => This::from_millis(ms * 1_000_000);
    pub fn from_secs(secs: u64): This => Duration(secs:, nanos: 0);

    pub fn as_nanos(this): u64 => this.secs * NS_PER_SEC + this.nanos as u64;
    pub fn as_micros(this): u64 => this.secs * 1_000_000 + this.nanos as u64 / 1_000;
    pub fn as_millis(this): u64 => this.secs * 1_000 + this.nanos as u64 / 1_000_000;

    pub fn as_secs(this): f64 => this.secs as f64 + this.nanos as f64 / NS_PER_SEC as f64;

    pub fn secs_part(this): u64 => this.secs;
    pub fn nanos_part(this): u32 => this.nanos;

    pub fn is_zero(this): bool => this == Duration::zero();

    pub fn ==(this, rhs: *This): bool => this.secs == rhs.secs and this.nanos == rhs.nanos;

    pub fn +(this, rhs: This): This {
        mut self = *this;
        self += rhs;
        self
    }

    pub fn +=(mut this, rhs: This) {
        this.secs += rhs.secs;
        this.nanos += rhs.nanos;
        if this.nanos > NS_PER_SEC as! u32 {
            this.nanos -= NS_PER_SEC as! u32;
            this.secs++;
        }
    }
}

pub struct Instant {
    ns: u64,

    pub fn now(): This {
        mut tp = libc::Timespec(tv_sec: 0, tv_nsec: 0);
        unsafe libc::clock_gettime(libc::CLOCK_MONOTONIC, &mut tp);
        Instant(ns: tp.tv_nsec as! u64 + tp.tv_sec as! u64 * 1_000_000_000)
    }

    pub fn duration_since(this, kw prev: This): Duration {
        guard this.ns.checked_sub(prev.ns) is ?val else {
            // TODO: monotonic time can go backwards on some platforms/systems
            panic("`prev` must be less than or equal to `this`");
        }

        Duration::from_nanos(val)
    }

    pub fn elapsed(my this): Duration => This::now().duration_since(prev: this);

    pub fn restart(mut this): Duration {
        let now = This::now();
        let elapsed = now.duration_since(prev: *this);
        *this = now;
        elapsed
    }

    pub fn <=>(this, rhs: *This): std::ops::Ordering => this.ns <=> rhs.ns;
    pub fn ==(this, rhs: *This): bool => this.ns == rhs.ns;

    impl std::ops::TotallyOrdered {}
}

pub fn sleep(dur: Duration) {
    mut spec = libc::Timespec(tv_sec: dur.secs as! c_long, tv_nsec: dur.nanos as! c_long);
    while unsafe libc::nanosleep(&spec, &mut spec) != 0 {
    }
}
