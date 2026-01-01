//! Constants Opcode Emitter
//!
//! Handles constant push opcodes:
//! - push_0 through push_7, push_minus1
//! - push_i8, push_i16, push_i32
//! - push_true, push_false
//! - push_this, undefined
//! - push_empty_string, push_atom_value
//! - push_const8
//! - object (creates empty object)

const std = @import("std");
const codegen = @import("../codegen_ssa.zig");
const parser = @import("../bytecode_parser.zig");

const SSACodeGen = codegen.SSACodeGen;
const Instruction = parser.Instruction;

/// Emit constant push opcodes. Returns true if the opcode was handled.
pub fn emit(self: *SSACodeGen, instr: Instruction, is_trampoline: bool) !bool {
    switch (instr.opcode) {
        .push_0 => {
            try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 0));\n");
            return true;
        },
        .push_1 => {
            try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 1));\n");
            return true;
        },
        .push_2 => {
            try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 2));\n");
            return true;
        },
        .push_3 => {
            try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 3));\n");
            return true;
        },
        .push_4 => {
            try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 4));\n");
            return true;
        },
        .push_5 => {
            try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 5));\n");
            return true;
        },
        .push_6 => {
            try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 6));\n");
            return true;
        },
        .push_7 => {
            try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, 7));\n");
            return true;
        },
        .push_minus1 => {
            try self.write("            PUSH(JS_MKVAL(JS_TAG_INT, -1));\n");
            return true;
        },
        .push_i8 => {
            try self.print("            PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i8});
            return true;
        },
        .push_i16 => {
            try self.print("            PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i16});
            return true;
        },
        .push_i32 => {
            try self.print("            PUSH(JS_MKVAL(JS_TAG_INT, {d}));\n", .{instr.operand.i32});
            return true;
        },
        .push_true => {
            try self.write("            PUSH(JS_TRUE);\n");
            return true;
        },
        .push_false => {
            try self.write("            PUSH(JS_FALSE);\n");
            return true;
        },
        .push_this => {
            try self.write("            PUSH(JS_DupValue(ctx, this_val));\n");
            return true;
        },
        .undefined => {
            try self.write("            PUSH(JS_UNDEFINED);\n");
            return true;
        },
        .push_empty_string => {
            try self.write("            PUSH(JS_NewString(ctx, \"\"));\n");
            return true;
        },
        .push_atom_value => {
            if (self.getAtomString(instr.operand.atom)) |name| {
                try self.write("            PUSH(JS_NewString(ctx, \"");
                try self.writeEscapedString(name);
                try self.write("\"));\n");
            } else {
                try self.write("            PUSH(JS_NewString(ctx, \"\"));\n");
            }
            return true;
        },
        .push_const8 => {
            try self.print("            if (_{s}_cpool && {d} < _{s}_cpool_count) {{\n", .{ self.options.func_name, instr.operand.const_idx, self.options.func_name });
            try self.print("                PUSH(JS_DupValue(ctx, _{s}_cpool[{d}]));\n", .{ self.options.func_name, instr.operand.const_idx });
            try self.write("            } else {\n");
            try self.write("                PUSH(JS_UNDEFINED);\n");
            try self.write("            }\n");
            return true;
        },
        .object => {
            try self.write("            PUSH(JS_NewObject(ctx));\n");
            return true;
        },
        .return_undef => {
            if (self.options.debug_comments) try self.write("            /* return_undef */\n");
            if (is_trampoline) {
                try self.write("            frame->result = JS_UNDEFINED; next_block = -1; break;\n");
            } else {
                try self.write("            return JS_UNDEFINED;\n");
            }
            return true;
        },
        else => return false,
    }
}
