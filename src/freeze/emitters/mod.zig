//! Opcode Emitters - Category-based opcode emission for SSA code generation
//!
//! Each module handles a specific category of JavaScript opcodes and generates
//! the corresponding C code for the frozen function compilation.
//!
//! Modules:
//! - arithmetic: add, sub, mul, div, mod, pow, neg, inc, dec, bitwise ops
//! - comparison: eq, neq, lt, lte, gt, gte, strict_eq, strict_neq
//! - stack: drop, dup, swap, rot, nip, insert, perm
//! - calls: call, call_method, call_constructor, apply, tail_call
//! - constants: push_0-7, push_i8/16/32, push_true/false/undefined/null

pub const arithmetic = @import("arithmetic.zig");
pub const comparison = @import("comparison.zig");
pub const stack = @import("stack.zig");
pub const calls = @import("calls.zig");
pub const constants = @import("constants.zig");
