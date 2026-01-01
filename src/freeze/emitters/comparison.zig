//! Comparison Opcode Emitter
//!
//! Handles comparison and type check opcodes:
//! - eq, neq, strict_eq, strict_neq
//! - lt, lte, gt, gte
//! - in, instanceof
//! - typeof, typeof_is_function, typeof_is_undefined
//! - is_null, is_undefined_or_null

const std = @import("std");
const codegen = @import("../codegen_ssa.zig");
const parser = @import("../bytecode_parser.zig");

const SSACodeGen = codegen.SSACodeGen;
const Instruction = parser.Instruction;

/// Emit comparison opcodes. Returns true if the opcode was handled.
pub fn emit(self: *SSACodeGen, instr: Instruction, is_trampoline: bool) !bool {
    switch (instr.opcode) {
        .eq => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
            return true;
        },
        .neq => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, !JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
            return true;
        },
        .strict_eq => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
            return true;
        },
        .strict_neq => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, !JS_IsStrictEqual(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
            return true;
        },
        .lt => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_lt(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
            return true;
        },
        .lte => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_lte(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
            return true;
        },
        .gt => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_gt(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
            return true;
        },
        .gte => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_NewBool(ctx, frozen_gte(ctx, a, b))); FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b); }\n");
            return true;
        },
        .in => {
            try self.write("            { JSValue rhs = POP(), lhs = POP();\n");
            try self.write("              int ret = JS_HasProperty(ctx, rhs, JS_ValueToAtom(ctx, lhs));\n");
            try self.write("              FROZEN_FREE(ctx, lhs); FROZEN_FREE(ctx, rhs);\n");
            try self.write("              ");
            try self.emitErrorCheck("ret < 0", is_trampoline);
            try self.write("              PUSH(JS_NewBool(ctx, ret)); }\n");
            return true;
        },
        .instanceof => {
            try self.write("            { JSValue rhs = POP(), lhs = POP();\n");
            try self.write("              int ret = JS_IsInstanceOf(ctx, lhs, rhs);\n");
            try self.write("              FROZEN_FREE(ctx, lhs); FROZEN_FREE(ctx, rhs);\n");
            try self.write("              ");
            try self.emitErrorCheck("ret < 0", is_trampoline);
            try self.write("              PUSH(JS_NewBool(ctx, ret)); }\n");
            return true;
        },
        .typeof => {
            try self.write("            { JSValue v = POP(); JSValue t = frozen_typeof(ctx, v); FROZEN_FREE(ctx, v); PUSH(t); }\n");
            return true;
        },
        .typeof_is_function => {
            try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsFunction(ctx, v))); FROZEN_FREE(ctx, v); }\n");
            return true;
        },
        .typeof_is_undefined => {
            try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsUndefined(v))); FROZEN_FREE(ctx, v); }\n");
            return true;
        },
        .is_null => {
            try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsNull(v))); FROZEN_FREE(ctx, v); }\n");
            return true;
        },
        .is_undefined_or_null => {
            try self.write("            { JSValue v = POP(); PUSH(JS_NewBool(ctx, JS_IsUndefined(v) || JS_IsNull(v))); FROZEN_FREE(ctx, v); }\n");
            return true;
        },
        else => return false,
    }
}
