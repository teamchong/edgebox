//! Stack Opcode Emitter
//!
//! Handles stack manipulation opcodes:
//! - drop, dup, dup1, dup2, dup3
//! - swap, swap2
//! - rot3l, rot3r, rot4l, rot5l
//! - nip, nip1
//! - insert2, insert3, insert4
//! - perm3, perm4, perm5

const std = @import("std");
const codegen = @import("../codegen_ssa.zig");
const parser = @import("../bytecode_parser.zig");

const SSACodeGen = codegen.SSACodeGen;
const Instruction = parser.Instruction;

/// Emit stack manipulation opcodes. Returns true if the opcode was handled.
pub fn emit(self: *SSACodeGen, instr: Instruction, is_trampoline: bool) !bool {
    _ = is_trampoline;
    switch (instr.opcode) {
        .drop => {
            try self.write("            FROZEN_FREE(ctx, POP());\n");
            return true;
        },
        .dup => {
            try self.write("            { JSValue tmp = TOP(); PUSH(FROZEN_DUP(ctx, tmp)); }\n");
            return true;
        },
        .dup1 => {
            try self.write("            { JSValue a = stack[sp-2]; JSValue b = stack[sp-1];\n");
            try self.write("              stack[sp] = b; stack[sp-1] = FROZEN_DUP(ctx, a); sp++; }\n");
            return true;
        },
        .dup2 => {
            try self.write("            { JSValue a = stack[sp-2], b = stack[sp-1]; PUSH(FROZEN_DUP(ctx, a)); PUSH(FROZEN_DUP(ctx, b)); }\n");
            return true;
        },
        .dup3 => {
            try self.write("            { JSValue a = stack[sp-3], b = stack[sp-2], c = stack[sp-1]; PUSH(FROZEN_DUP(ctx, a)); PUSH(FROZEN_DUP(ctx, b)); PUSH(FROZEN_DUP(ctx, c)); }\n");
            return true;
        },
        .swap => {
            try self.write("            { JSValue b = stack[sp-1], a = stack[sp-2]; stack[sp-2] = b; stack[sp-1] = a; }\n");
            return true;
        },
        .swap2 => {
            try self.write("            { JSValue d = stack[sp-1], c = stack[sp-2], b = stack[sp-3], a = stack[sp-4]; stack[sp-4] = c; stack[sp-3] = d; stack[sp-2] = a; stack[sp-1] = b; }\n");
            return true;
        },
        .rot3l => {
            try self.write("            { JSValue c = stack[sp-1], b = stack[sp-2], a = stack[sp-3]; stack[sp-3] = b; stack[sp-2] = c; stack[sp-1] = a; }\n");
            return true;
        },
        .rot3r => {
            try self.write("            { JSValue c = stack[sp-1], b = stack[sp-2], a = stack[sp-3]; stack[sp-3] = c; stack[sp-2] = a; stack[sp-1] = b; }\n");
            return true;
        },
        .rot4l => {
            try self.write("            { JSValue d = stack[sp-1], c = stack[sp-2], b = stack[sp-3], a = stack[sp-4]; stack[sp-4] = b; stack[sp-3] = c; stack[sp-2] = d; stack[sp-1] = a; }\n");
            return true;
        },
        .rot5l => {
            try self.write("            { JSValue e = stack[sp-1], d = stack[sp-2], c = stack[sp-3], b = stack[sp-4], a = stack[sp-5]; stack[sp-5] = b; stack[sp-4] = c; stack[sp-3] = d; stack[sp-2] = e; stack[sp-1] = a; }\n");
            return true;
        },
        .nip => {
            try self.write("            { JSValue top = POP(); FROZEN_FREE(ctx, POP()); PUSH(top); }\n");
            return true;
        },
        .nip1 => {
            try self.write("            { JSValue top = POP(); JSValue second = POP(); FROZEN_FREE(ctx, POP()); PUSH(second); PUSH(top); }\n");
            return true;
        },
        .insert2 => {
            try self.write("            { JSValue v = POP(); JSValue a = stack[sp-1]; stack[sp-1] = v; PUSH(a); }\n");
            return true;
        },
        .insert3 => {
            try self.write("            { JSValue v = POP(); JSValue b = stack[sp-1]; JSValue a = stack[sp-2]; stack[sp-2] = v; stack[sp-1] = a; PUSH(b); }\n");
            return true;
        },
        .insert4 => {
            try self.write("            { JSValue v = POP(); JSValue c = stack[sp-1]; JSValue b = stack[sp-2]; JSValue a = stack[sp-3]; stack[sp-3] = v; stack[sp-2] = a; stack[sp-1] = b; PUSH(c); }\n");
            return true;
        },
        .perm3 => {
            try self.write("            { JSValue c = stack[sp-1], b = stack[sp-2], a = stack[sp-3]; stack[sp-3] = b; stack[sp-2] = c; stack[sp-1] = a; }\n");
            return true;
        },
        .perm4 => {
            try self.write("            { JSValue d = stack[sp-1], c = stack[sp-2], b = stack[sp-3], a = stack[sp-4]; stack[sp-4] = b; stack[sp-3] = c; stack[sp-2] = d; stack[sp-1] = a; }\n");
            return true;
        },
        .perm5 => {
            try self.write("            { JSValue e = stack[sp-1], d = stack[sp-2], c = stack[sp-3], b = stack[sp-4], a = stack[sp-5]; stack[sp-5] = b; stack[sp-4] = c; stack[sp-3] = d; stack[sp-2] = e; stack[sp-1] = a; }\n");
            return true;
        },
        else => return false,
    }
}
