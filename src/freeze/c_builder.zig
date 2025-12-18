const std = @import("std");
const Allocator = std.mem.Allocator;
const codegen_ssa = @import("codegen_ssa.zig");

/// Re-export OutputLanguage from codegen_ssa to avoid duplication
pub const OutputLanguage = codegen_ssa.OutputLanguage;

/// Execution mode context for code generation
pub const CodeGenContext = struct {
    is_trampoline: bool,
    language: OutputLanguage,
    stack_var: []const u8, // "stack" or "frame->stack"
    locals_var: []const u8, // "locals" or "frame->locals"
    sp_var: []const u8, // "sp" or "frame->sp"

    pub fn init(is_trampoline: bool, language: OutputLanguage) CodeGenContext {
        return .{
            .is_trampoline = is_trampoline,
            .language = language,
            .stack_var = if (is_trampoline) "frame->stack" else "stack",
            .locals_var = if (is_trampoline) "frame->locals" else "locals",
            .sp_var = if (is_trampoline) "frame->sp" else "sp",
        };
    }
};

/// Represents a C expression/value with ownership tracking
pub const CValue = struct {
    expr: []const u8,
    allocator: Allocator,
    owned: bool,

    pub fn init(allocator: Allocator, expr: []const u8) CValue {
        return .{
            .expr = expr,
            .allocator = allocator,
            .owned = false,
        };
    }

    pub fn initOwned(allocator: Allocator, expr: []const u8) CValue {
        return .{
            .expr = expr,
            .allocator = allocator,
            .owned = true,
        };
    }

    pub fn deinit(self: CValue) void {
        if (self.owned) {
            self.allocator.free(self.expr);
        }
    }

    /// Cast to a different type
    pub fn cast(self: CValue, allocator: Allocator, type_name: []const u8) !CValue {
        const result = try std.fmt.allocPrint(allocator, "(({s}){s})", .{ type_name, self.expr });
        return CValue.initOwned(allocator, result);
    }

    /// Dereference pointer
    pub fn deref(self: CValue, allocator: Allocator) !CValue {
        const result = try std.fmt.allocPrint(allocator, "(*{s})", .{self.expr});
        return CValue.initOwned(allocator, result);
    }

    /// Access struct field with dot operator
    pub fn field(self: CValue, allocator: Allocator, field_name: []const u8) !CValue {
        const result = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ self.expr, field_name });
        return CValue.initOwned(allocator, result);
    }

    /// Access struct field with arrow operator
    pub fn arrow(self: CValue, allocator: Allocator, field_name: []const u8) !CValue {
        const result = try std.fmt.allocPrint(allocator, "{s}->{s}", .{ self.expr, field_name });
        return CValue.initOwned(allocator, result);
    }

    /// Array/pointer index access
    pub fn index(self: CValue, allocator: Allocator, idx: anytype) !CValue {
        const T = @TypeOf(idx);
        const type_info = @typeInfo(T);

        // Handle integers
        if (type_info == .int or type_info == .comptime_int) {
            const idx_str = try std.fmt.allocPrint(allocator, "{d}", .{idx});
            defer allocator.free(idx_str);
            const result = try std.fmt.allocPrint(allocator, "{s}[{s}]", .{ self.expr, idx_str });
            return CValue.initOwned(allocator, result);
        }

        // Handle pointer types (includes []const u8, []u8, *const [N]u8, etc.)
        if (type_info == .pointer) {
            const result = try std.fmt.allocPrint(allocator, "{s}[{s}]", .{ self.expr, idx });
            return CValue.initOwned(allocator, result);
        }

        // Handle CValue
        if (T == CValue) {
            const result = try std.fmt.allocPrint(allocator, "{s}[{s}]", .{ self.expr, idx.expr });
            return CValue.initOwned(allocator, result);
        }

        @compileError("Unsupported index type: " ++ @typeName(T));
    }

    /// Function call
    pub fn call(self: CValue, allocator: Allocator, args: []const CValue) !CValue {
        var args_list = std.ArrayList(u8){};
        defer args_list.deinit(allocator);
        const writer = args_list.writer(allocator);

        for (args, 0..) |arg, i| {
            if (i > 0) try writer.writeAll(", ");
            try writer.writeAll(arg.expr);
        }

        const result = try std.fmt.allocPrint(allocator, "{s}({s})", .{ self.expr, args_list.items });
        return CValue.initOwned(allocator, result);
    }
};

/// RAII-style block that auto-closes when dropped
pub const CBlock = struct {
    builder: *CBuilder,
    closed: bool = false,

    pub fn close(self: *CBlock) !void {
        if (self.closed) return;
        self.closed = true;
        self.builder.indent_level -= 1;
        try self.builder.writeIndent();
        try self.builder.output.appendSlice(self.builder.allocator, "}\n");
    }

    pub fn deinit(self: *CBlock) void {
        self.close() catch {};
    }
};

/// Main C code builder with high-level API
pub const CBuilder = struct {
    allocator: Allocator,
    output: std.ArrayList(u8),
    indent_level: u32 = 0,
    context: CodeGenContext,

    pub fn init(allocator: Allocator, context: CodeGenContext) CBuilder {
        return .{
            .allocator = allocator,
            .output = std.ArrayList(u8){},
            .context = context,
        };
    }

    pub fn deinit(self: *CBuilder) void {
        self.output.deinit(self.allocator);
    }

    pub fn getOutput(self: *CBuilder) []const u8 {
        return self.output.items;
    }

    pub fn reset(self: *CBuilder) void {
        self.output.clearRetainingCapacity();
        self.indent_level = 0;
    }

    fn writeIndent(self: *CBuilder) !void {
        var i: u32 = 0;
        while (i < self.indent_level) : (i += 1) {
            try self.output.appendSlice(self.allocator, "  ");
        }
    }

    pub fn writeLine(self: *CBuilder, line: []const u8) !void {
        try self.writeIndent();
        try self.output.appendSlice(self.allocator, line);
        try self.output.append(self.allocator, '\n');
    }

    pub fn writeLineNoIndent(self: *CBuilder, line: []const u8) !void {
        try self.output.appendSlice(self.allocator, line);
        try self.output.append(self.allocator, '\n');
    }

    pub fn write(self: *CBuilder, text: []const u8) !void {
        try self.output.appendSlice(self.allocator, text);
    }

    pub fn print(self: *CBuilder, comptime fmt: []const u8, args: anytype) !void {
        try std.fmt.format(self.output.writer(self.allocator), fmt, args);
    }

    /// Begin a generic block with custom header
    pub fn beginBlock(self: *CBuilder, header: []const u8) !CBlock {
        try self.writeIndent();
        try self.output.appendSlice(self.allocator, header);
        try self.output.appendSlice(self.allocator, " {\n");
        self.indent_level += 1;
        return CBlock{ .builder = self };
    }

    /// Begin if block
    pub fn beginIf(self: *CBuilder, condition: CValue) !CBlock {
        const header = try std.fmt.allocPrint(self.allocator, "if ({s})", .{condition.expr});
        defer self.allocator.free(header);
        return self.beginBlock(header);
    }

    /// Begin else if block
    pub fn beginElseIf(self: *CBuilder, condition: CValue) !CBlock {
        const header = try std.fmt.allocPrint(self.allocator, "else if ({s})", .{condition.expr});
        defer self.allocator.free(header);
        return self.beginBlock(header);
    }

    /// Begin else block
    pub fn beginElse(self: *CBuilder) !CBlock {
        return self.beginBlock("else");
    }

    /// Begin while loop
    pub fn beginWhile(self: *CBuilder, condition: CValue) !CBlock {
        const header = try std.fmt.allocPrint(self.allocator, "while ({s})", .{condition.expr});
        defer self.allocator.free(header);
        return self.beginBlock(header);
    }

    /// Begin for loop
    pub fn beginFor(self: *CBuilder, init_expr: []const u8, condition: []const u8, increment: []const u8) !CBlock {
        const header = try std.fmt.allocPrint(self.allocator, "for ({s}; {s}; {s})", .{ init_expr, condition, increment });
        defer self.allocator.free(header);
        return self.beginBlock(header);
    }

    /// Begin switch statement
    pub fn beginSwitch(self: *CBuilder, expr: CValue) !CBlock {
        const header = try std.fmt.allocPrint(self.allocator, "switch ({s})", .{expr.expr});
        defer self.allocator.free(header);
        return self.beginBlock(header);
    }

    /// Begin scope block
    pub fn beginScope(self: *CBuilder) !CBlock {
        try self.writeIndent();
        try self.output.appendSlice(self.allocator, "{\n");
        self.indent_level += 1;
        return CBlock{ .builder = self };
    }

    /// Emit a case label
    pub fn emitCase(self: *CBuilder, value: anytype) !void {
        try self.writeIndent();
        switch (@TypeOf(value)) {
            comptime_int, i32, u32, usize => try self.print("case {d}:\n", .{value}),
            []const u8 => try self.print("case {s}:\n", .{value}),
            else => @compileError("Unsupported case value type"),
        }
        self.indent_level += 1;
    }

    /// Emit default case
    pub fn emitDefault(self: *CBuilder) !void {
        try self.writeLine("default:");
        self.indent_level += 1;
    }

    /// Emit return statement
    pub fn emitReturn(self: *CBuilder, value: ?CValue) !void {
        if (value) |v| {
            const line = try std.fmt.allocPrint(self.allocator, "return {s};", .{v.expr});
            defer self.allocator.free(line);
            try self.writeLine(line);
        } else {
            try self.writeLine("return;");
        }
    }

    /// Emit break statement (only valid in trampoline mode within switch)
    pub fn emitBreak(self: *CBuilder) !void {
        try self.writeLine("break;");
    }

    /// Emit continue statement
    pub fn emitContinue(self: *CBuilder) !void {
        try self.writeLine("continue;");
    }

    /// Emit goto statement
    pub fn emitGoto(self: *CBuilder, label: []const u8) !void {
        const line = try std.fmt.allocPrint(self.allocator, "goto {s};", .{label});
        defer self.allocator.free(line);
        try self.writeLine(line);
    }

    /// Emit label
    pub fn emitLabel(self: *CBuilder, label: []const u8) !void {
        const line = try std.fmt.allocPrint(self.allocator, "{s}:", .{label});
        defer self.allocator.free(line);
        try self.writeLine(line);
    }

    /// Emit variable declaration
    pub fn emitVarDecl(self: *CBuilder, type_name: []const u8, var_name: []const u8, init_value: ?CValue) !void {
        if (init_value) |v| {
            const line = try std.fmt.allocPrint(self.allocator, "{s} {s} = {s};", .{ type_name, var_name, v.expr });
            defer self.allocator.free(line);
            try self.writeLine(line);
        } else {
            const line = try std.fmt.allocPrint(self.allocator, "{s} {s};", .{ type_name, var_name });
            defer self.allocator.free(line);
            try self.writeLine(line);
        }
    }

    /// Emit assignment statement
    pub fn emitAssign(self: *CBuilder, lhs: CValue, rhs: CValue) !void {
        const line = try std.fmt.allocPrint(self.allocator, "{s} = {s};", .{ lhs.expr, rhs.expr });
        defer self.allocator.free(line);
        try self.writeLine(line);
    }

    /// Emit exception check (common pattern in freeze codegen)
    pub fn emitExceptionCheck(self: *CBuilder, value: CValue) !void {
        try self.emitExceptionCheckWithCleanup(value, null);
    }

    /// Emit exception check with optional cleanup code before return/break
    pub fn emitExceptionCheckWithCleanup(self: *CBuilder, value: CValue, cleanup: ?[]const u8) !void {
        const check = CValue.init(self.allocator, "JS_IsException");
        const call = try check.call(self.allocator, &[_]CValue{value});
        defer call.deinit();

        var block = try self.beginIf(call);
        defer block.deinit();

        // Emit cleanup code if provided
        if (cleanup) |c| {
            try self.writeLine(c);
        }

        if (self.context.is_trampoline) {
            try self.writeLine("next_block = -1;");
            const frame_result = try std.fmt.allocPrint(self.allocator, "frame->result = {s};", .{value.expr});
            defer self.allocator.free(frame_result);
            try self.writeLine(frame_result);
            try self.emitBreak();
        } else {
            try self.emitReturn(value);
        }
    }

    /// Emit error code check (for functions returning <0 on error)
    pub fn emitErrorCheck(self: *CBuilder, condition: CValue) !void {
        var block = try self.beginIf(condition);
        defer block.deinit();

        if (self.context.is_trampoline) {
            try self.writeLine("next_block = -1;");
            try self.writeLine("frame->result = JS_EXCEPTION;");
            try self.emitBreak();
        } else {
            try self.writeLine("return JS_EXCEPTION;");
        }
    }

    /// Emit throw with custom throw function (JS_ThrowTypeError, JS_ThrowReferenceError, etc.)
    pub fn emitThrow(self: *CBuilder, throw_func: []const u8, message: []const u8) !void {
        const throw_call = try std.fmt.allocPrint(self.allocator, "{s}(ctx, \"{s}\")", .{ throw_func, message });
        defer self.allocator.free(throw_call);

        if (self.context.is_trampoline) {
            try self.writeLine("next_block = -1;");
            const frame_result = try std.fmt.allocPrint(self.allocator, "frame->result = {s};", .{throw_call});
            defer self.allocator.free(frame_result);
            try self.writeLine(frame_result);
            try self.emitBreak();
        } else {
            const ret = try std.fmt.allocPrint(self.allocator, "return {s};", .{throw_call});
            defer self.allocator.free(ret);
            try self.writeLine(ret);
        }
    }

    /// Emit throw type error (common pattern for type checks)
    pub fn emitThrowTypeError(self: *CBuilder, message: []const u8) !void {
        try self.emitThrow("JS_ThrowTypeError", message);
    }

    /// Emit throw reference error (for TDZ checks)
    pub fn emitThrowReferenceError(self: *CBuilder, message: []const u8) !void {
        try self.emitThrow("JS_ThrowReferenceError", message);
    }

    /// Emit binary operation (common pattern for arithmetic/comparison)
    pub fn emitBinaryOp(
        self: *CBuilder,
        result_var: []const u8,
        op_func: []const u8,
        left: CValue,
        right: CValue,
    ) !void {
        const op_call = CValue.init(self.allocator, op_func);
        const result = try op_call.call(self.allocator, &[_]CValue{ CValue.init(self.allocator, "ctx"), left, right });
        defer result.deinit();

        const line = try std.fmt.allocPrint(self.allocator, "{s} = {s};", .{ result_var, result.expr });
        defer self.allocator.free(line);
        try self.writeLine(line);
    }

    /// Emit stack push operation
    pub fn emitStackPush(self: *CBuilder, value: CValue) !void {
        const sp_inc = try std.fmt.allocPrint(self.allocator, "{s}++;", .{self.context.sp_var});
        defer self.allocator.free(sp_inc);
        try self.writeLine(sp_inc);

        const stack_idx = CValue.init(self.allocator, self.context.stack_var);
        const sp_minus_1 = try std.fmt.allocPrint(self.allocator, "{s} - 1", .{self.context.sp_var});
        defer self.allocator.free(sp_minus_1);
        const stack_assign = try stack_idx.index(self.allocator, sp_minus_1);
        defer stack_assign.deinit();

        try self.emitAssign(stack_assign, value);
    }

    /// Emit stack pop operation
    pub fn emitStackPop(self: *CBuilder, result_var: []const u8) !void {
        const sp_dec = try std.fmt.allocPrint(self.allocator, "{s}--;", .{self.context.sp_var});
        defer self.allocator.free(sp_dec);

        const stack_idx = CValue.init(self.allocator, self.context.stack_var);
        const stack_top = try stack_idx.index(self.allocator, self.context.sp_var);
        defer stack_top.deinit();

        const line = try std.fmt.allocPrint(self.allocator, "{s} = {s};", .{ result_var, stack_top.expr });
        defer self.allocator.free(line);
        try self.writeLine(line);
        try self.writeLine(sp_dec);
    }

    /// Emit comment
    pub fn emitComment(self: *CBuilder, comment: []const u8) !void {
        const line = try std.fmt.allocPrint(self.allocator, "// {s}", .{comment});
        defer self.allocator.free(line);
        try self.writeLine(line);
    }

    /// Get local variable expression (context-aware: locals[idx] or frame->locals[idx])
    pub fn getLocalExpr(self: *CBuilder, idx: u32) ![]const u8 {
        return try std.fmt.allocPrint(self.allocator, "{s}[{d}]", .{ self.context.locals_var, idx });
    }

    /// Emit put_loc operation: free old value, store new value from stack
    pub fn emitPutLoc(self: *CBuilder, idx: u32) !void {
        var scope = try self.beginScope();
        const local_expr = try self.getLocalExpr(idx);
        defer self.allocator.free(local_expr);
        const free_line = try std.fmt.allocPrint(self.allocator, "FROZEN_FREE(ctx, {s});", .{local_expr});
        defer self.allocator.free(free_line);
        try self.writeLine(free_line);
        const assign_line = try std.fmt.allocPrint(self.allocator, "{s} = POP();", .{local_expr});
        defer self.allocator.free(assign_line);
        try self.writeLine(assign_line);
        try scope.close();
    }

    /// Emit get_loc operation: push local value onto stack (with dup)
    pub fn emitGetLoc(self: *CBuilder, idx: u32) !void {
        const local_expr = try self.getLocalExpr(idx);
        defer self.allocator.free(local_expr);
        const push_line = try std.fmt.allocPrint(self.allocator, "PUSH(FROZEN_DUP(ctx, {s}));", .{local_expr});
        defer self.allocator.free(push_line);
        try self.writeLine(push_line);
    }

    /// Emit set_loc operation: free old value, store dup of top (doesn't pop)
    pub fn emitSetLoc(self: *CBuilder, idx: u32) !void {
        var scope = try self.beginScope();
        const local_expr = try self.getLocalExpr(idx);
        defer self.allocator.free(local_expr);
        const free_line = try std.fmt.allocPrint(self.allocator, "FROZEN_FREE(ctx, {s});", .{local_expr});
        defer self.allocator.free(free_line);
        try self.writeLine(free_line);
        const assign_line = try std.fmt.allocPrint(self.allocator, "{s} = FROZEN_DUP(ctx, TOP());", .{local_expr});
        defer self.allocator.free(assign_line);
        try self.writeLine(assign_line);
        try scope.close();
    }

    /// Emit set_loc_uninitialized: mark local as uninitialized (for TDZ)
    pub fn emitSetLocUninitialized(self: *CBuilder, idx: u32) !void {
        const local_expr = try self.getLocalExpr(idx);
        defer self.allocator.free(local_expr);
        const line = try std.fmt.allocPrint(self.allocator, "{s} = JS_UNINITIALIZED;", .{local_expr});
        defer self.allocator.free(line);
        try self.writeLine(line);
    }

    /// Emit inc_loc operation: increment local by 1
    pub fn emitIncLoc(self: *CBuilder, idx: u32) !void {
        var scope = try self.beginScope();
        const local_expr = try self.getLocalExpr(idx);
        defer self.allocator.free(local_expr);
        const old_decl = try std.fmt.allocPrint(self.allocator, "JSValue old = {s};", .{local_expr});
        defer self.allocator.free(old_decl);
        try self.writeLine(old_decl);
        const assign = try std.fmt.allocPrint(self.allocator, "{s} = frozen_add(ctx, old, JS_MKVAL(JS_TAG_INT, 1));", .{local_expr});
        defer self.allocator.free(assign);
        try self.writeLine(assign);
        try scope.close();
    }

    /// Emit dec_loc operation: decrement local by 1
    pub fn emitDecLoc(self: *CBuilder, idx: u32) !void {
        var scope = try self.beginScope();
        const local_expr = try self.getLocalExpr(idx);
        defer self.allocator.free(local_expr);
        const old_decl = try std.fmt.allocPrint(self.allocator, "JSValue old = {s};", .{local_expr});
        defer self.allocator.free(old_decl);
        try self.writeLine(old_decl);
        const assign = try std.fmt.allocPrint(self.allocator, "{s} = frozen_sub(ctx, old, JS_MKVAL(JS_TAG_INT, 1));", .{local_expr});
        defer self.allocator.free(assign);
        try self.writeLine(assign);
        try scope.close();
    }

    /// Emit add_loc operation: pop value, add to local (locals[idx] += POP())
    pub fn emitAddLoc(self: *CBuilder, idx: u32) !void {
        var scope = try self.beginScope();
        const local_expr = try self.getLocalExpr(idx);
        defer self.allocator.free(local_expr);
        // JSValue v = POP(), old = locals[idx];
        try self.writeLine("JSValue v = POP();");
        const old_decl = try std.fmt.allocPrint(self.allocator, "JSValue old = {s};", .{local_expr});
        defer self.allocator.free(old_decl);
        try self.writeLine(old_decl);
        // locals[idx] = frozen_add(ctx, old, v);
        const assign = try std.fmt.allocPrint(self.allocator, "{s} = frozen_add(ctx, old, v);", .{local_expr});
        defer self.allocator.free(assign);
        try self.writeLine(assign);
        try scope.close();
    }

    /// Emit get_loc operation with arbitrary index: PUSH(FROZEN_DUP(ctx, locals[idx]))
    pub fn emitGetLocN(self: *CBuilder, idx: u32) !void {
        const local_expr = try self.getLocalExpr(idx);
        defer self.allocator.free(local_expr);
        const line = try std.fmt.allocPrint(self.allocator, "PUSH(FROZEN_DUP(ctx, {s}));", .{local_expr});
        defer self.allocator.free(line);
        try self.writeLine(line);
    }

    /// Emit put_loc operation with arbitrary index: { FROZEN_FREE(ctx, locals[idx]); locals[idx] = POP(); }
    pub fn emitPutLocN(self: *CBuilder, idx: u32) !void {
        var scope = try self.beginScope();
        const local_expr = try self.getLocalExpr(idx);
        defer self.allocator.free(local_expr);
        const free_line = try std.fmt.allocPrint(self.allocator, "FROZEN_FREE(ctx, {s});", .{local_expr});
        defer self.allocator.free(free_line);
        try self.writeLine(free_line);
        const assign_line = try std.fmt.allocPrint(self.allocator, "{s} = POP();", .{local_expr});
        defer self.allocator.free(assign_line);
        try self.writeLine(assign_line);
        try scope.close();
    }

    /// Emit set_loc operation with arbitrary index: { FROZEN_FREE(ctx, locals[idx]); locals[idx] = FROZEN_DUP(ctx, TOP()); }
    pub fn emitSetLocN(self: *CBuilder, idx: u32) !void {
        var scope = try self.beginScope();
        const local_expr = try self.getLocalExpr(idx);
        defer self.allocator.free(local_expr);
        const free_line = try std.fmt.allocPrint(self.allocator, "FROZEN_FREE(ctx, {s});", .{local_expr});
        defer self.allocator.free(free_line);
        try self.writeLine(free_line);
        const assign_line = try std.fmt.allocPrint(self.allocator, "{s} = FROZEN_DUP(ctx, TOP());", .{local_expr});
        defer self.allocator.free(assign_line);
        try self.writeLine(assign_line);
        try scope.close();
    }

    // ==================== PROPERTY ACCESS HELPERS ====================

    /// Emit get_field: { JSValue obj = POP(); JSValue val = JS_GetPropertyStr(ctx, obj, "name"); FROZEN_FREE(ctx, obj); if (JS_IsException(val)) <error>; PUSH(val); }
    pub fn emitGetField(self: *CBuilder, name: []const u8) !void {
        var scope = try self.beginScope();
        try self.writeLine("JSValue obj = POP();");
        const get_line = try std.fmt.allocPrint(self.allocator, "JSValue val = JS_GetPropertyStr(ctx, obj, \"{s}\");", .{name});
        defer self.allocator.free(get_line);
        try self.writeLine(get_line);
        try self.writeLine("FROZEN_FREE(ctx, obj);");

        // Exception check
        const val = CValue.init(self.allocator, "val");
        try self.emitExceptionCheck(val);

        try self.writeLine("PUSH(val);");
        try scope.close();
    }

    /// Emit get_field2: { JSValue obj = TOP(); JSValue val = JS_GetPropertyStr(ctx, obj, "name"); if (JS_IsException(val)) <error>; PUSH(val); }
    /// get_field2 doesn't pop obj - it's used for method calls where we need both obj and method
    pub fn emitGetField2(self: *CBuilder, name: []const u8) !void {
        var scope = try self.beginScope();
        try self.writeLine("JSValue obj = TOP();");
        const get_line = try std.fmt.allocPrint(self.allocator, "JSValue val = JS_GetPropertyStr(ctx, obj, \"{s}\");", .{name});
        defer self.allocator.free(get_line);
        try self.writeLine(get_line);

        // Exception check
        const val = CValue.init(self.allocator, "val");
        try self.emitExceptionCheck(val);

        try self.writeLine("PUSH(val);");
        try scope.close();
    }

    /// Emit put_field: { JSValue val = POP(); JSValue obj = POP(); int ret = JS_SetPropertyStr(ctx, obj, "name", val); FROZEN_FREE(ctx, obj); if (ret < 0) <error>; }
    pub fn emitPutField(self: *CBuilder, name: []const u8) !void {
        var scope = try self.beginScope();
        try self.writeLine("JSValue val = POP();");
        try self.writeLine("JSValue obj = POP();");
        const set_line = try std.fmt.allocPrint(self.allocator, "int ret = JS_SetPropertyStr(ctx, obj, \"{s}\", val);", .{name});
        defer self.allocator.free(set_line);
        try self.writeLine(set_line);
        try self.writeLine("FROZEN_FREE(ctx, obj);");

        // Error check (ret < 0 means error)
        const condition = CValue.init(self.allocator, "ret < 0");
        try self.emitErrorCheck(condition);

        try scope.close();
    }

    /// Emit get_array_el: uses frozen_array_get to handle both int and string keys (for for-in)
    pub fn emitGetArrayEl(self: *CBuilder) !void {
        var scope = try self.beginScope();
        try self.writeLine("JSValue idx = POP();");
        try self.writeLine("JSValue arr = POP();");
        try self.writeLine("JSValue ret = frozen_array_get(ctx, arr, idx);");
        try self.writeLine("FROZEN_FREE(ctx, arr);");
        try self.writeLine("FROZEN_FREE(ctx, idx);");

        const ret = CValue.init(self.allocator, "ret");
        try self.emitExceptionCheck(ret);

        try self.writeLine("PUSH(ret);");
        try scope.close();
    }

    /// Emit put_array_el: uses frozen_array_set to handle both int and string keys
    pub fn emitPutArrayEl(self: *CBuilder) !void {
        var scope = try self.beginScope();
        try self.writeLine("JSValue val = POP();");
        try self.writeLine("JSValue idx = POP();");
        try self.writeLine("JSValue arr = POP();");
        try self.writeLine("int ret = frozen_array_set(ctx, arr, idx, val);");
        try self.writeLine("FROZEN_FREE(ctx, arr);");
        try self.writeLine("FROZEN_FREE(ctx, idx);");

        const condition = CValue.init(self.allocator, "ret < 0");
        try self.emitErrorCheck(condition);

        try scope.close();
    }

    /// Emit simple stack push with undefined fallback (for empty property names)
    pub fn emitPushUndefined(self: *CBuilder) !void {
        try self.writeLine("PUSH(JS_UNDEFINED);");
    }

    /// Emit free and push undefined (for invalid property access)
    pub fn emitFreeAndPushUndefined(self: *CBuilder) !void {
        var scope = try self.beginScope();
        try self.writeLine("JSValue obj = POP();");
        try self.writeLine("FROZEN_FREE(ctx, obj);");
        try self.writeLine("PUSH(JS_UNDEFINED);");
        try scope.close();
    }
};
