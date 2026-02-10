mod rng;

pub use rng::Rng;

pub union ControlFlow {
    Continue,
    Break,
}

pub union ControlFlowWithValue<T> {
    Continue,
    Break(T),
}
