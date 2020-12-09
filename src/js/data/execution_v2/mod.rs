use crate::js::data::execution_v2::stack_element::StackElement;

pub mod function;
pub mod opcode;
pub mod stack_element;
pub mod stack_executor;
pub mod var;

pub struct Stack {
    values: Vec<StackElement>,
    current_function: usize,
}
