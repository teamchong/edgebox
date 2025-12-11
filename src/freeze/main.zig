//! edgebox-freeze CLI
//! Bytecode to C transpiler for frozen functions

const std = @import("std");
const opcodes = @import("opcodes.zig");
const parser = @import("bytecode_parser.zig");
const cfg_builder = @import("cfg_builder.zig");
const codegen = @import("codegen.zig");
const codegen_ssa = @import("codegen_ssa.zig");
const module_parser = @import("module_parser.zig");

const BytecodeParser = parser.BytecodeParser;
const Instruction = parser.Instruction;
const ModuleParser = module_parser.ModuleParser;
const SSACodeGen = codegen_ssa.SSACodeGen;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage(args[0]);
        return;
    }

    // Parse command line arguments
    var input_file: ?[]const u8 = null;
    var output_file: []const u8 = "frozen_output.c";
    var func_name: []const u8 = "frozen_func";
    var debug_mode = false;
    var disasm_mode = false;
    var ssa_mode = true; // Default to SSA codegen for better performance
    var arg_count: u16 = 0;
    var var_count: u16 = 0;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Error: -o requires an argument\n", .{});
                return;
            }
            output_file = args[i];
        } else if (std.mem.eql(u8, arg, "-n") or std.mem.eql(u8, arg, "--name")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Error: -n requires an argument\n", .{});
                return;
            }
            func_name = args[i];
        } else if (std.mem.eql(u8, arg, "-a") or std.mem.eql(u8, arg, "--args")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Error: -a requires an argument\n", .{});
                return;
            }
            arg_count = std.fmt.parseInt(u16, args[i], 10) catch {
                std.debug.print("Error: invalid arg count\n", .{});
                return;
            };
        } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--vars")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("Error: -v requires an argument\n", .{});
                return;
            }
            var_count = std.fmt.parseInt(u16, args[i], 10) catch {
                std.debug.print("Error: invalid var count\n", .{});
                return;
            };
        } else if (std.mem.eql(u8, arg, "-d") or std.mem.eql(u8, arg, "--debug")) {
            debug_mode = true;
        } else if (std.mem.eql(u8, arg, "--disasm")) {
            disasm_mode = true;
        } else if (std.mem.eql(u8, arg, "--legacy")) {
            ssa_mode = false;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            printUsage(args[0]);
            return;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            input_file = arg;
        } else {
            std.debug.print("Error: unknown option: {s}\n", .{arg});
            return;
        }
    }

    if (input_file == null) {
        std.debug.print("Error: no input file specified\n", .{});
        printUsage(args[0]);
        return;
    }

    // Read input file
    const file_content = readBytecodeFile(allocator, input_file.?) catch |err| {
        std.debug.print("Error reading input file '{s}': {}\n", .{ input_file.?, err });
        return;
    };
    defer allocator.free(file_content);

    std.debug.print("Read {d} bytes of bytecode\n", .{file_content.len});

    // Try to parse as QuickJS module format first
    var mod_parser = ModuleParser.init(allocator, file_content);
    defer mod_parser.deinit();

    var bytecode: []const u8 = file_content;
    var extracted_arg_count: u16 = arg_count;
    var extracted_var_count: u16 = var_count;
    var extracted_stack_size: u16 = 256;

    mod_parser.parse() catch |err| {
        std.debug.print("Module parse error (may be raw bytecode): {}\n", .{err});
    };

    // If we found functions, use the one with args (typically the user function, not the module wrapper)
    if (mod_parser.functions.items.len > 0) {
        // Find the best function - prefer functions with arguments
        var best_idx: usize = 0;
        for (mod_parser.functions.items, 0..) |func, idx| {
            std.debug.print("  Function {d}: args={d} vars={d} bc_len={d}\n", .{ idx, func.arg_count, func.var_count, func.bytecode.len });
            // Prefer functions with arguments (actual user functions)
            if (func.arg_count > 0) {
                best_idx = idx;
                break;
            }
        }
        const func_info = mod_parser.functions.items[best_idx];
        bytecode = func_info.bytecode;
        extracted_arg_count = @intCast(func_info.arg_count);
        extracted_var_count = @intCast(func_info.var_count);
        extracted_stack_size = @intCast(func_info.stack_size);
        std.debug.print("Using function {d}: args={d} vars={d} stack={d} bytecode_len={d}\n", .{
            best_idx,
            extracted_arg_count,
            extracted_var_count,
            extracted_stack_size,
            bytecode.len,
        });
    }

    // Use command-line overrides if provided
    if (arg_count > 0) extracted_arg_count = arg_count;
    if (var_count > 0) extracted_var_count = var_count;

    // Parse bytecode
    var bc_parser = BytecodeParser.init(bytecode);
    const instructions = bc_parser.parseAll(allocator) catch |err| {
        std.debug.print("Error parsing bytecode: {}\n", .{err});
        return;
    };
    defer allocator.free(instructions);

    std.debug.print("Parsed {d} instructions\n", .{instructions.len});

    // Disassembly mode
    if (disasm_mode) {
        std.debug.print("\n=== Disassembly ===\n", .{});
        for (instructions) |instr| {
            const info = instr.getInfo();
            std.debug.print("{d:>4}: {s}\n", .{ instr.pc, info.name });
        }
        return;
    }

    // Check if function can be frozen
    const freeze_check = parser.canFreezeFunction(instructions);
    if (!freeze_check.can_freeze) {
        std.debug.print("Warning: Function contains unfrozen opcode: {s}\n", .{freeze_check.reason.?});
        std.debug.print("Some operations will fall back to runtime error.\n", .{});
    }

    // Build CFG
    var cfg = cfg_builder.buildCFG(allocator, instructions) catch |err| {
        std.debug.print("Error building CFG: {}\n", .{err});
        return;
    };
    defer cfg.deinit();

    std.debug.print("Built CFG with {d} basic blocks\n", .{cfg.blocks.items.len});

    if (debug_mode) {
        // Use stderr for debug output (std.debug.print goes to stderr)
        cfg.dumpDebug();
    }

    // Generate C code and write to file
    if (ssa_mode) {
        // SSA-based codegen - generates optimized native int32 code
        var gen = SSACodeGen.init(allocator, &cfg, .{
            .func_name = func_name,
            .debug_comments = debug_mode,
            .arg_count = extracted_arg_count,
            .var_count = extracted_var_count,
        });
        defer gen.deinit();

        const code = gen.generate() catch |err| {
            std.debug.print("Error generating SSA code: {}\n", .{err});
            return;
        };

        writeOutputFile(output_file, code) catch return;
        std.debug.print("Generated {d} bytes of C code to '{s}'\n", .{ code.len, output_file });
    } else {
        // Legacy codegen - JSValue stack simulation
        var gen = codegen.CodeGenerator.init(allocator, &cfg, .{
            .func_name = func_name,
            .debug_comments = debug_mode,
            .arg_count = extracted_arg_count,
            .var_count = extracted_var_count,
            .max_stack = extracted_stack_size,
        });
        defer gen.deinit();

        const code = gen.generate() catch |err| {
            std.debug.print("Error generating code: {}\n", .{err});
            return;
        };

        writeOutputFile(output_file, code) catch return;
        std.debug.print("Generated {d} bytes of C code to '{s}'\n", .{ code.len, output_file });
    }
}

fn writeOutputFile(path: []const u8, content: []const u8) !void {
    const file = std.fs.cwd().createFile(path, .{}) catch |err| {
        std.debug.print("Error creating output file '{s}': {}\n", .{ path, err });
        return err;
    };
    defer file.close();

    file.writeAll(content) catch |err| {
        std.debug.print("Error writing output file: {}\n", .{err});
        return err;
    };
}

fn printUsage(prog: []const u8) void {
    std.debug.print(
        \\Usage: {s} [options] <bytecode_file>
        \\
        \\Freeze QuickJS bytecode into optimized C code.
        \\
        \\Options:
        \\  -o, --output <file>   Output C file (default: frozen_output.c)
        \\  -n, --name <name>     Function name (default: frozen_func)
        \\  -a, --args <count>    Number of arguments
        \\  -v, --vars <count>    Number of local variables
        \\  -d, --debug           Include debug comments
        \\  --disasm              Disassemble bytecode only
        \\  -h, --help            Show this help
        \\
        \\Input file format:
        \\  Raw bytecode binary, or C array from qjsc -e output
        \\
        \\Example:
        \\  # Generate bytecode with qjsc
        \\  qjsc -e -o fib.c fib.js
        \\
        \\  # Extract bytecode and freeze
        \\  {s} fib.bc -o frozen_fib.c -n frozen_fib -a 1
        \\
        \\  # Or disassemble first
        \\  {s} fib.bc --disasm
        \\
    , .{ prog, prog, prog });
}

/// Read bytecode from file
/// Supports both raw binary and C array format from qjsc
fn readBytecodeFile(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 10 * 1024 * 1024); // 10MB max
    defer allocator.free(content);

    // Check if it's a C array format
    if (std.mem.indexOf(u8, content, "const uint8_t") != null or
        std.mem.indexOf(u8, content, "static const uint8_t") != null)
    {
        return parseCArrayBytecode(allocator, content);
    }

    // Raw binary
    return allocator.dupe(u8, content);
}

/// Parse bytecode from C array format (qjsc -e output)
fn parseCArrayBytecode(allocator: std.mem.Allocator, content: []const u8) ![]u8 {
    var bytes = std.ArrayListUnmanaged(u8){};
    errdefer bytes.deinit(allocator);

    // Find the array body between { and }
    const start = std.mem.indexOf(u8, content, "{") orelse return error.InvalidFormat;
    const end = std.mem.lastIndexOf(u8, content, "}") orelse return error.InvalidFormat;

    if (start >= end) return error.InvalidFormat;

    const array_content = content[start + 1 .. end];

    // Parse hex bytes: 0xNN, 0xNN, ...
    var i: usize = 0;
    while (i < array_content.len) {
        // Skip whitespace and commas
        while (i < array_content.len and (array_content[i] == ' ' or
            array_content[i] == '\n' or
            array_content[i] == '\r' or
            array_content[i] == '\t' or
            array_content[i] == ','))
        {
            i += 1;
        }

        if (i >= array_content.len) break;

        // Parse 0xNN
        if (i + 3 < array_content.len and
            array_content[i] == '0' and
            (array_content[i + 1] == 'x' or array_content[i + 1] == 'X'))
        {
            const hex = array_content[i + 2 .. i + 4];
            const byte = std.fmt.parseInt(u8, hex, 16) catch {
                i += 1;
                continue;
            };
            try bytes.append(allocator, byte);
            i += 4;
        } else {
            i += 1;
        }
    }

    return bytes.toOwnedSlice(allocator);
}

// Tests
test "parse C array bytecode" {
    const c_array =
        \\const uint8_t bundle[] = {
        \\    0x01, 0x02, 0x03, 0x04,
        \\    0xab, 0xcd, 0xef,
        \\};
    ;

    const bytes = try parseCArrayBytecode(std.testing.allocator, c_array);
    defer std.testing.allocator.free(bytes);

    try std.testing.expectEqual(@as(usize, 7), bytes.len);
    try std.testing.expectEqual(@as(u8, 0x01), bytes[0]);
    try std.testing.expectEqual(@as(u8, 0xef), bytes[6]);
}
