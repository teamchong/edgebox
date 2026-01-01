//! Call Opcode Emitter
//!
//! Handles function call opcodes:
//! - call, call0, call1, call2, call3
//! - call_method, call_constructor
//! - apply, tail_call_method

const std = @import("std");
const codegen = @import("../codegen_ssa.zig");
const parser = @import("../bytecode_parser.zig");

const SSACodeGen = codegen.SSACodeGen;
const Instruction = parser.Instruction;

/// Emit call opcodes. Returns true if the opcode was handled.
pub fn emit(self: *SSACodeGen, instr: Instruction, is_trampoline: bool) !bool {
    switch (instr.opcode) {
        .call => {
            const argc: u16 = instr.operand.u16;
            try self.write("            {\n");
            if (argc > 0) {
                try self.print("              JSValue args[{d}];\n", .{argc});
                var i = argc;
                while (i > 0) {
                    i -= 1;
                    try self.print("              args[{d}] = POP();\n", .{i});
                }
            }
            try self.write("              JSValue func = POP();\n");
            if (argc > 0) {
                try self.print("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, {d}, args);\n", .{argc});
            } else {
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 0, NULL);\n");
            }
            try self.write("              FROZEN_FREE(ctx, func);\n");
            if (argc > 0) {
                var j: u16 = 0;
                while (j < argc) : (j += 1) {
                    try self.print("              FROZEN_FREE(ctx, args[{d}]);\n", .{j});
                }
            }
            try self.emitExceptionCheck("ret", is_trampoline);
            try self.write("              PUSH(ret);\n");
            try self.write("            }\n");
            self.pending_self_call = false;
            return true;
        },
        .call0 => {
            try self.write("            { JSValue func = POP();\n");
            try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 0, NULL);\n");
            try self.write("              FROZEN_FREE(ctx, func);\n");
            try self.emitExceptionCheck("ret", is_trampoline);
            try self.write("              PUSH(ret); }\n");
            self.pending_self_call = false;
            return true;
        },
        .call1 => {
            if (self.pending_self_call and self.options.is_self_recursive and self.options.use_direct_recursion) {
                try self.write("            { JSValue arg0 = POP();\n");
                try self.print("              JSValue new_argv[1] = {{arg0}};\n", .{});
                try self.print("              JSValue ret = {s}(ctx, JS_UNDEFINED, 1, new_argv);\n", .{self.options.func_name});
                try self.write("              FROZEN_FREE(ctx, arg0);\n");
                try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              PUSH(ret); }\n");
            } else {
                try self.write("            { JSValue arg0 = POP(); JSValue func = POP();\n");
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 1, &arg0);\n");
                try self.write("              FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, arg0);\n");
                try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              PUSH(ret); }\n");
            }
            self.pending_self_call = false;
            return true;
        },
        .call2 => {
            if (self.pending_self_call and self.options.is_self_recursive and self.options.use_direct_recursion) {
                try self.write("            { JSValue args[2]; args[1] = POP(); args[0] = POP();\n");
                try self.print("              JSValue ret = {s}(ctx, JS_UNDEFINED, 2, args);\n", .{self.options.func_name});
                try self.write("              FROZEN_FREE(ctx, args[0]); FROZEN_FREE(ctx, args[1]);\n");
                try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              PUSH(ret); }\n");
            } else {
                try self.write("            { JSValue args[2]; args[1] = POP(); args[0] = POP(); JSValue func = POP();\n");
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 2, args);\n");
                try self.write("              FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, args[0]); FROZEN_FREE(ctx, args[1]);\n");
                try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              PUSH(ret); }\n");
            }
            self.pending_self_call = false;
            return true;
        },
        .call3 => {
            if (self.pending_self_call and self.options.is_self_recursive and self.options.use_direct_recursion) {
                try self.write("            { JSValue args[3]; args[2] = POP(); args[1] = POP(); args[0] = POP();\n");
                try self.print("              JSValue ret = {s}(ctx, JS_UNDEFINED, 3, args);\n", .{self.options.func_name});
                try self.write("              FROZEN_FREE(ctx, args[0]); FROZEN_FREE(ctx, args[1]); FROZEN_FREE(ctx, args[2]);\n");
                try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              PUSH(ret); }\n");
            } else {
                try self.write("            { JSValue args[3]; args[2] = POP(); args[1] = POP(); args[0] = POP(); JSValue func = POP();\n");
                try self.write("              JSValue ret = JS_Call(ctx, func, JS_UNDEFINED, 3, args);\n");
                try self.write("              FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, args[0]); FROZEN_FREE(ctx, args[1]); FROZEN_FREE(ctx, args[2]);\n");
                try self.emitExceptionCheck("ret", is_trampoline);
                try self.write("              PUSH(ret); }\n");
            }
            self.pending_self_call = false;
            return true;
        },
        .call_method => {
            const argc = instr.operand.u16;
            try self.write("            {{\n");
            if (argc > 0) {
                try self.print("              JSValue args[{d}];\n", .{argc});
                var i = argc;
                while (i > 0) {
                    i -= 1;
                    try self.print("              args[{d}] = POP();\n", .{i});
                }
            }
            try self.write("              JSValue func = POP();\n");
            try self.write("              JSValue this_obj = POP();\n");
            if (argc > 0) {
                try self.print("              JSValue result = JS_Call(ctx, func, this_obj, {d}, args);\n", .{argc});
            } else {
                try self.write("              JSValue result = JS_Call(ctx, func, this_obj, 0, NULL);\n");
            }
            try self.write("              FROZEN_FREE(ctx, func);\n");
            try self.write("              FROZEN_FREE(ctx, this_obj);\n");
            if (argc > 0) {
                var j: u16 = 0;
                while (j < argc) : (j += 1) {
                    try self.print("              FROZEN_FREE(ctx, args[{d}]);\n", .{j});
                }
            }
            if (is_trampoline) {
                try self.write("              if (JS_IsException(result)) {{ next_block = -1; frame->result = result; break; }}\n");
            } else {
                try self.write("              if (JS_IsException(result)) return result;\n");
            }
            try self.write("              PUSH(result);\n");
            try self.write("            }}\n");
            return true;
        },
        .call_constructor => {
            const argc: u16 = instr.operand.u16;
            try self.print("            {{ JSValue args[{d} > 0 ? {d} : 1]; ", .{ argc, argc });
            var i: u16 = argc;
            while (i > 0) {
                i -= 1;
                try self.print("args[{d}] = POP(); ", .{i});
            }
            try self.write("JSValue ctor = POP();\n");
            try self.print("              JSValue ret = JS_CallConstructor(ctx, ctor, {d}, args);\n", .{argc});
            try self.write("              FROZEN_FREE(ctx, ctor);");
            i = 0;
            while (i < argc) : (i += 1) {
                try self.print(" FROZEN_FREE(ctx, args[{d}]);", .{i});
            }
            if (is_trampoline) {
                try self.write("\n              if (JS_IsException(ret)) {{ next_block = -1; frame->result = ret; break; }}\n");
            } else {
                try self.write("\n              if (JS_IsException(ret)) return ret;\n");
            }
            try self.write("              PUSH(ret); }\n");
            self.pending_self_call = false;
            return true;
        },
        .apply => {
            try self.write("            { JSValue args_array = POP(); JSValue this_obj = POP(); JSValue func = POP();\n");
            try self.write("              JSValue len_val = JS_GetPropertyStr(ctx, args_array, \"length\");\n");
            try self.write("              int64_t argc = 0;\n");
            try self.write("              JS_ToInt64(ctx, &argc, len_val);\n");
            try self.write("              JS_FreeValue(ctx, len_val);\n");
            try self.write("              JSValue *argv = NULL;\n");
            try self.write("              if (argc > 0) {\n");
            try self.write("                argv = js_malloc(ctx, argc * sizeof(JSValue));\n");
            try self.write("                for (int i = 0; i < argc; i++) {\n");
            try self.write("                  argv[i] = JS_GetPropertyUint32(ctx, args_array, i);\n");
            try self.write("                }\n");
            try self.write("              }\n");
            try self.write("              FROZEN_FREE(ctx, args_array);\n");
            try self.write("              JSValue result = JS_Call(ctx, func, this_obj, (int)argc, argv);\n");
            try self.write("              FROZEN_FREE(ctx, func); FROZEN_FREE(ctx, this_obj);\n");
            try self.write("              for (int i = 0; i < argc; i++) { JS_FreeValue(ctx, argv[i]); }\n");
            try self.write("              if (argv) js_free(ctx, argv);\n");
            try self.emitExceptionCheck("result", is_trampoline);
            try self.write("              PUSH(result); }\n");
            return true;
        },
        .tail_call_method => {
            const argc: u16 = instr.operand.u16;
            try self.print("            {{ JSValue args[{d} > 0 ? {d} : 1]; ", .{ argc, argc });
            var i: u16 = argc;
            while (i > 0) {
                i -= 1;
                try self.print("args[{d}] = POP(); ", .{i});
            }
            try self.write("JSValue method = POP(); JSValue this_obj = POP();\n");
            try self.print("              JSValue ret = JS_Call(ctx, method, this_obj, {d}, args);\n", .{argc});
            try self.write("              FROZEN_FREE(ctx, method); FROZEN_FREE(ctx, this_obj);");
            i = 0;
            while (i < argc) : (i += 1) {
                try self.print(" FROZEN_FREE(ctx, args[{d}]);", .{i});
            }
            try self.write("\n              ");
            try self.emitExceptionCheck("ret", is_trampoline);
            if (is_trampoline) {
                try self.write("              frame->result = ret; next_block = -1; break; }\n");
            } else {
                try self.write("              return ret; }\n");
            }
            self.pending_self_call = false;
            return true;
        },
        else => return false,
    }
}
