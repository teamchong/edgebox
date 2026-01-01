//! Arithmetic Opcode Emitter
//!
//! Handles arithmetic and bitwise operation opcodes:
//! - add, sub, mul, div, mod, pow, neg, plus
//! - inc, dec
//! - shl, shr, sar, xor

const std = @import("std");
const codegen = @import("../codegen_ssa.zig");
const parser = @import("../bytecode_parser.zig");

const SSACodeGen = codegen.SSACodeGen;
const Instruction = parser.Instruction;

/// Emit arithmetic opcodes. Returns true if the opcode was handled.
pub fn emit(self: *SSACodeGen, instr: Instruction, is_trampoline: bool) !bool {
    switch (instr.opcode) {
        .add => {
            try self.write(
                \\            { JSValue b = POP(), a = POP();
                \\              if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {
                \\                  int64_t sum = (int64_t)JS_VALUE_GET_INT(a) + JS_VALUE_GET_INT(b);
                \\                  if (likely(sum >= INT32_MIN && sum <= INT32_MAX)) {
                \\                      PUSH(JS_MKVAL(JS_TAG_INT, (int32_t)sum));
                \\                  } else {
                \\                      PUSH(JS_NewFloat64(ctx, (double)sum));
                \\                  }
                \\              } else {
                \\                  JSValue r = frozen_add(ctx, a, b);
                \\                  FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);
                \\
            );
            try self.emitExceptionCheck("r", is_trampoline);
            try self.write(
                \\                  PUSH(r);
                \\              }
                \\            }
                \\
            );
            return true;
        },
        .sub => {
            try self.write("            { JSValue b = POP(), a = POP();\n");
            try self.write("              if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {\n");
            try self.write("                  int64_t diff = (int64_t)JS_VALUE_GET_INT(a) - JS_VALUE_GET_INT(b);\n");
            try self.write("                  if (likely(diff >= INT32_MIN && diff <= INT32_MAX)) {\n");
            try self.write("                      PUSH(JS_MKVAL(JS_TAG_INT, (int32_t)diff));\n");
            try self.write("                  } else {\n");
            try self.write("                      PUSH(JS_NewFloat64(ctx, (double)diff));\n");
            try self.write("                  }\n");
            try self.write("              } else {\n");
            try self.write("                  JSValue r = frozen_sub(ctx, a, b);\n");
            try self.write("                  FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);\n");
            if (is_trampoline) {
                try self.write("                  if (JS_IsException(r)) { next_block = -1; frame->result = r; break; }\n");
            } else {
                try self.write("                  if (JS_IsException(r)) return r;\n");
            }
            try self.write("                  PUSH(r);\n");
            try self.write("              } }\n");
            return true;
        },
        .mul => {
            try self.write("            { JSValue b = POP(), a = POP();\n");
            try self.write("              if (likely(JS_VALUE_GET_TAG(a) == JS_TAG_INT && JS_VALUE_GET_TAG(b) == JS_TAG_INT)) {\n");
            try self.write("                  int64_t prod = (int64_t)JS_VALUE_GET_INT(a) * JS_VALUE_GET_INT(b);\n");
            try self.write("                  if (likely(prod >= INT32_MIN && prod <= INT32_MAX)) {\n");
            try self.write("                      PUSH(JS_MKVAL(JS_TAG_INT, (int32_t)prod));\n");
            try self.write("                  } else {\n");
            try self.write("                      PUSH(JS_NewFloat64(ctx, (double)prod));\n");
            try self.write("                  }\n");
            try self.write("              } else {\n");
            try self.write("                  JSValue r = frozen_mul(ctx, a, b);\n");
            try self.write("                  FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);\n");
            if (is_trampoline) {
                try self.write("                  if (JS_IsException(r)) { next_block = -1; frame->result = r; break; }\n");
            } else {
                try self.write("                  if (JS_IsException(r)) return r;\n");
            }
            try self.write("                  PUSH(r);\n");
            try self.write("              } }\n");
            return true;
        },
        .div => {
            try self.write("            { JSValue b = POP(), a = POP();\n");
            try self.write("              JSValue r = frozen_div(ctx, a, b);\n");
            try self.write("              FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);\n");
            if (is_trampoline) {
                try self.write("              if (JS_IsException(r)) { next_block = -1; frame->result = r; break; }\n");
            } else {
                try self.write("              if (JS_IsException(r)) return r;\n");
            }
            try self.write("              PUSH(r); }\n");
            return true;
        },
        .mod => {
            try self.write("            { JSValue b = POP(), a = POP();\n");
            try self.write("              JSValue r = frozen_mod(ctx, a, b);\n");
            try self.write("              FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);\n");
            if (is_trampoline) {
                try self.write("              if (JS_IsException(r)) { next_block = -1; frame->result = r; break; }\n");
            } else {
                try self.write("              if (JS_IsException(r)) return r;\n");
            }
            try self.write("              PUSH(r); }\n");
            return true;
        },
        .pow => {
            try self.write("            { JSValue b = POP(), a = POP();\n");
            try self.write("              double da, db;\n");
            try self.write("              JS_ToFloat64(ctx, &da, a); JS_ToFloat64(ctx, &db, b);\n");
            try self.write("              FROZEN_FREE(ctx, a); FROZEN_FREE(ctx, b);\n");
            try self.write("              PUSH(JS_NewFloat64(ctx, pow(da, db))); }\n");
            return true;
        },
        .neg => {
            try self.write("            { JSValue a = POP(); JSValue r = frozen_neg(ctx, a); FROZEN_FREE(ctx, a); if (JS_IsException(r)) return r; PUSH(r); }\n");
            return true;
        },
        .plus => {
            try self.write("            { JSValue v = POP(); JSValue r = JS_ToNumber(ctx, v); FROZEN_FREE(ctx, v);\n");
            try self.write("              ");
            try self.emitExceptionCheck("r", is_trampoline);
            try self.write("              PUSH(r); }\n");
            return true;
        },
        .inc => {
            try self.write("            { JSValue a = POP(); if (JS_VALUE_GET_TAG(a) == JS_TAG_INT) { PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) + 1)); } else { PUSH(JS_NewFloat64(ctx, JS_VALUE_GET_FLOAT64(JS_ToNumber(ctx, a)) + 1)); } FROZEN_FREE(ctx, a); }\n");
            return true;
        },
        .dec => {
            try self.write("            { JSValue a = POP(); if (JS_VALUE_GET_TAG(a) == JS_TAG_INT) { PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) - 1)); } else { PUSH(JS_NewFloat64(ctx, JS_VALUE_GET_FLOAT64(JS_ToNumber(ctx, a)) - 1)); } FROZEN_FREE(ctx, a); }\n");
            return true;
        },
        .shl => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) << (JS_VALUE_GET_INT(b) & 31))); }\n");
            return true;
        },
        .shr => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, (int32_t)((uint32_t)JS_VALUE_GET_INT(a) >> (JS_VALUE_GET_INT(b) & 31)))); }\n");
            return true;
        },
        .sar => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) >> (JS_VALUE_GET_INT(b) & 31))); }\n");
            return true;
        },
        .xor => {
            try self.write("            { JSValue b = POP(), a = POP(); PUSH(JS_MKVAL(JS_TAG_INT, JS_VALUE_GET_INT(a) ^ JS_VALUE_GET_INT(b))); }\n");
            return true;
        },
        else => return false,
    }
}
