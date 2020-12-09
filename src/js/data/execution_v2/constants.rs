/// Global stack layout:
/// 0: global object
/// 1..n: (Local Stack)*

/// Local stack layout:
/// 0: FunctionHead
/// 1: this
/// 2: args
/// 3: return-to-value (set when a called method returns back to ours)
/// 4..n: variables

pub const HEAD_LOCATION: usize = 0;
pub const THIS_LOCATION: usize = 1;
pub const ARGS_LOCATION: usize = 2;
pub const RETURN_TO_LOCATION: usize = 3;
pub const JUMP_FLAG_LOCATION: usize = 4;
pub const BEGIN_VARS: usize = 5;
