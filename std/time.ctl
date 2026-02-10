use std::deps::{libc, libc::posix::*};

const NS_PER_SEC: u64 = 1_000_000_000;

pub struct Duration {
    secs: u64,
    nanos: u32,

    pub fn new(kw mut secs: u64, kw nanos: u32): This {
        mut dur = This::from_nanos(nanos as u64);
        dur.secs += secs;
        dur
    }
    pub fn zero(): This => This(secs: 0, nanos: 0);

    pub fn from_nanos(ns: u64): This => This(secs: ns / NS_PER_SEC, nanos: (ns % NS_PER_SEC).cast());
    pub fn from_micros(us: u64): This => This::from_nanos(us * 1_000);
    pub fn from_millis(ms: u64): This => This::from_nanos(ms * 1_000_000);
    pub fn from_secs(secs: u64): This => This(secs:, nanos: 0);

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
        if this.nanos > NS_PER_SEC.cast() {
            this.nanos -= NS_PER_SEC.cast();
            this.secs++;
        }
    }
}

pub struct Instant {
    ns: u64,

    pub fn now(): This {
        mut tp = libc::timespec(tv_sec: 0, tv_nsec: 0);
        clock_gettime(CLOCK_MONOTONIC, &mut tp);
        Instant(ns: tp.tv_nsec.cast() + tp.tv_sec.cast() * 1_000_000_000)
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

    pub fn checked_add(this, dur: Duration): ?This {
        This(ns: this.ns.checked_add(dur.as_nanos())?)
    }

    pub fn checked_sub(this, dur: Duration): ?This {
        This(ns: this.ns.checked_add(dur.as_nanos())?)
    }

    pub fn +(this, dur: Duration): This => this.checked_add(dur)!;
    pub fn -(this, dur: Duration): This => this.checked_sub(dur)!;

    pub fn +=(mut this, dur: Duration) => *this = this + dur;
    pub fn -=(mut this, dur: Duration) => *this = this - dur;

    impl std::ops::TotallyOrdered {}
}

pub fn sleep(dur: Duration) {
    mut spec = libc::timespec(tv_sec: dur.secs.saturating_cast(), tv_nsec: dur.nanos.cast());
    while clock_nanosleep(CLOCK_MONOTONIC, 0, &spec, &mut spec) is res and res != 0 {
        assert_eq(res, libc::EINTR);
    }
}

pub fn sleep_until(deadline: Instant) {
    let dur = Duration::from_nanos(deadline.ns);
    // TODO: Handle this better for 32 bit platforms
    let spec = libc::timespec(tv_sec: dur.secs.saturating_cast(), tv_nsec: dur.nanos.cast());
    while clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &spec, null) is res and res != 0 {
        assert_eq(res, libc::EINTR);
    }
}
