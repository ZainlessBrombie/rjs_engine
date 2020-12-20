// TODO I'll focus on optimizations once the language is complete or nearly complete
/*
pub fn optimize(ops: Vec<Op>) -> Vec<Op> {
    if ops.len() == 0 {
        return ops;
    }
    let start: *const Op = ops.first().unwrap();
    let mut next: HashMap<*const Op, (*const Op, *const Op)> = Default::default();
    let reads: HashMap<*const Op, *const usize> = Default::default();
    let assigns: HashMap<*const Op, *const usize> = Default::default();

    for (i, x) in ops.iter().enumerate() {
        match &x.code {
            OpCode::Jump { to } => {
                next.insert(x as *const Op, (0 as *const Op, *to as *const Op));
            }
            OpCode::ConditionalJump { to } => {
                next.insert(
                    x as *const Op,
                    (
                        ops.get(i + 1)
                            .map(|r| r as *const Op)
                            .unwrap_or(0 as *const Op),
                        *to as *const Op,
                    ),
                );
            }
            _ => {
                next.insert(
                    x as *const Op,
                    (
                        ops.get(i + 1)
                            .map(|r| r as *const Op)
                            .unwrap_or(0 as *const Op),
                        0 as *const Op,
                    ),
                );
            }
        }
    }

    unimplemented!()
}
*/
