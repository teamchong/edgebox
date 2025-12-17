const std = @import("std");
const testing = std.testing;
const c_builder = @import("c_builder.zig");
const CBuilder = c_builder.CBuilder;
const CValue = c_builder.CValue;
const CodeGenContext = c_builder.CodeGenContext;
const OutputLanguage = c_builder.OutputLanguage;

test "CValue - basic init" {
    const allocator = testing.allocator;
    const val = CValue.init(allocator, "foo");
    defer val.deinit();
    try testing.expectEqualStrings("foo", val.expr);
}

test "CValue - cast" {
    const allocator = testing.allocator;
    const val = CValue.init(allocator, "x");
    const casted = try val.cast(allocator, "int32_t");
    defer casted.deinit();
    try testing.expectEqualStrings("((int32_t)x)", casted.expr);
}

test "CValue - field access" {
    const allocator = testing.allocator;
    const val = CValue.init(allocator, "obj");
    const field = try val.field(allocator, "count");
    defer field.deinit();
    try testing.expectEqualStrings("obj.count", field.expr);
}

test "CValue - arrow access" {
    const allocator = testing.allocator;
    const val = CValue.init(allocator, "ptr");
    const field = try val.arrow(allocator, "data");
    defer field.deinit();
    try testing.expectEqualStrings("ptr->data", field.expr);
}

test "CValue - array index" {
    const allocator = testing.allocator;
    const val = CValue.init(allocator, "arr");
    const indexed = try val.index(allocator, 5);
    defer indexed.deinit();
    try testing.expectEqualStrings("arr[5]", indexed.expr);
}

test "CValue - function call" {
    const allocator = testing.allocator;
    const func = CValue.init(allocator, "foo");
    const arg1 = CValue.init(allocator, "a");
    const arg2 = CValue.init(allocator, "b");
    const call = try func.call(allocator, &[_]CValue{ arg1, arg2 });
    defer call.deinit();
    try testing.expectEqualStrings("foo(a, b)", call.expr);
}

test "CBuilder - basic output" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(false, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    try builder.writeLine("int x = 42;");
    const output = builder.getOutput();
    try testing.expectEqualStrings("int x = 42;\n", output);
}

test "CBuilder - if block" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(false, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    const cond = CValue.init(allocator, "x > 0");
    var block = try builder.beginIf(cond);
    try builder.writeLine("return 1;");
    try block.close();

    const expected =
        \\if (x > 0) {
        \\  return 1;
        \\}
        \\
    ;
    try testing.expectEqualStrings(expected, builder.getOutput());
}

test "CBuilder - nested blocks" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(false, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    const cond1 = CValue.init(allocator, "x > 0");
    var block1 = try builder.beginIf(cond1);

    const cond2 = CValue.init(allocator, "y > 0");
    var block2 = try builder.beginIf(cond2);
    try builder.writeLine("return 1;");
    try block2.close();

    try block1.close();

    const expected =
        \\if (x > 0) {
        \\  if (y > 0) {
        \\    return 1;
        \\  }
        \\}
        \\
    ;
    try testing.expectEqualStrings(expected, builder.getOutput());
}

test "CBuilder - switch statement" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(true, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    const expr = CValue.init(allocator, "opcode");
    var switch_block = try builder.beginSwitch(expr);

    try builder.emitCase(1);
    try builder.writeLine("do_something();");
    try builder.emitBreak();
    builder.indent_level -= 1;

    try builder.emitCase(2);
    try builder.writeLine("do_other();");
    try builder.emitBreak();
    builder.indent_level -= 1;

    try builder.emitDefault();
    try builder.emitBreak();
    builder.indent_level -= 1;

    try switch_block.close();

    const expected =
        \\switch (opcode) {
        \\  case 1:
        \\    do_something();
        \\    break;
        \\  case 2:
        \\    do_other();
        \\    break;
        \\  default:
        \\    break;
        \\}
        \\
    ;
    try testing.expectEqualStrings(expected, builder.getOutput());
}

test "CBuilder - variable declaration" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(false, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    const init_val = CValue.init(allocator, "42");
    try builder.emitVarDecl("int", "x", init_val);
    try builder.emitVarDecl("int", "y", null);

    const expected =
        \\int x = 42;
        \\int y;
        \\
    ;
    try testing.expectEqualStrings(expected, builder.getOutput());
}

test "CBuilder - exception check non-trampoline" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(false, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    const val = CValue.init(allocator, "result");
    try builder.emitExceptionCheck(val);

    const expected =
        \\if (JS_IsException(result)) {
        \\  return result;
        \\}
        \\
    ;
    try testing.expectEqualStrings(expected, builder.getOutput());
}

test "CBuilder - exception check trampoline" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(true, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    const val = CValue.init(allocator, "result");
    try builder.emitExceptionCheck(val);

    const expected =
        \\if (JS_IsException(result)) {
        \\  next_block = -1;
        \\  frame->result = result;
        \\  break;
        \\}
        \\
    ;
    try testing.expectEqualStrings(expected, builder.getOutput());
}

test "CBuilder - stack push" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(false, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    const val = CValue.init(allocator, "JS_NewInt32(ctx, 42)");
    try builder.emitStackPush(val);

    const expected =
        \\sp++;
        \\stack[sp - 1] = JS_NewInt32(ctx, 42);
        \\
    ;
    try testing.expectEqualStrings(expected, builder.getOutput());
}

test "CBuilder - stack push trampoline" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(true, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    const val = CValue.init(allocator, "JS_NewInt32(ctx, 42)");
    try builder.emitStackPush(val);

    const expected =
        \\frame->sp++;
        \\frame->stack[frame->sp - 1] = JS_NewInt32(ctx, 42);
        \\
    ;
    try testing.expectEqualStrings(expected, builder.getOutput());
}

test "CBuilder - stack pop" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(false, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    try builder.emitStackPop("val");

    const expected =
        \\val = stack[sp];
        \\sp--;
        \\
    ;
    try testing.expectEqualStrings(expected, builder.getOutput());
}

test "CBuilder - binary operation" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(false, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    const left = CValue.init(allocator, "a");
    const right = CValue.init(allocator, "b");
    try builder.emitBinaryOp("result", "frozen_add", left, right);

    const expected = "result = frozen_add(ctx, a, b);\n";
    try testing.expectEqualStrings(expected, builder.getOutput());
}

test "CodeGenContext - non-trampoline" {
    const ctx = CodeGenContext.init(false, .c);
    try testing.expect(!ctx.is_trampoline);
    try testing.expectEqualStrings("stack", ctx.stack_var);
    try testing.expectEqualStrings("locals", ctx.locals_var);
    try testing.expectEqualStrings("sp", ctx.sp_var);
}

test "CodeGenContext - trampoline" {
    const ctx = CodeGenContext.init(true, .c);
    try testing.expect(ctx.is_trampoline);
    try testing.expectEqualStrings("frame->stack", ctx.stack_var);
    try testing.expectEqualStrings("frame->locals", ctx.locals_var);
    try testing.expectEqualStrings("frame->sp", ctx.sp_var);
}

test "CBuilder - RAII cleanup" {
    const allocator = testing.allocator;
    const ctx = CodeGenContext.init(false, .c);
    var builder = CBuilder.init(allocator, ctx);
    defer builder.deinit();

    {
        const cond = CValue.init(allocator, "x > 0");
        var block = try builder.beginIf(cond);
        defer block.deinit(); // Auto-close via RAII
        try builder.writeLine("return 1;");
        // Block closes here automatically
    }

    const expected =
        \\if (x > 0) {
        \\  return 1;
        \\}
        \\
    ;
    try testing.expectEqualStrings(expected, builder.getOutput());
}
