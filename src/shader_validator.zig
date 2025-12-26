// WGSL Shader Validator
//
// Validates WebGPU Shading Language (WGSL) shaders before execution:
// - Instruction complexity estimation
// - Feature allowlist enforcement
// - Dangerous pattern detection
// - Memory access bounds estimation
// - Recursion/infinite loop detection
//
// This runs in the main process BEFORE sending to GPU worker,
// providing defense-in-depth against malicious shaders.
//
// Security Model:
// 1. Lexical analysis catches obvious issues (deep loops, banned features)
// 2. Complexity estimation prevents GPU stalls
// 3. wgpu-native's naga compiler provides final validation
// 4. GPU worker runs in isolated process with resource limits

const std = @import("std");
const gpu_sandbox = @import("gpu_sandbox.zig");

pub const ValidationError = error{
    InvalidSyntax,
    TooComplex,
    DisallowedFeature,
    InfiniteLoop,
    ExcessiveMemory,
    UnsupportedBuiltin,
    DangerousPattern,
    UnbalancedBraces,
    InvalidWorkgroupSize,
};

pub const ValidationResult = struct {
    is_valid: bool,
    estimated_instructions: u32,
    estimated_memory: u32, // bytes
    error_message: ?[]const u8,
    line_number: ?u32,
    features_used: FeatureSet,
    warnings: std.BoundedArray(Warning, 16),

    pub const FeatureSet = struct {
        uses_compute: bool = false,
        uses_vertex: bool = false,
        uses_fragment: bool = false,
        uses_storage_textures: bool = false,
        uses_atomics: bool = false,
        uses_texture_sample: bool = false,
        uses_derivative: bool = false,
        loop_count: u32 = 0,
        max_loop_depth: u32 = 0,
        workgroup_size: [3]u32 = .{ 1, 1, 1 },
        buffer_count: u32 = 0,
        texture_count: u32 = 0,
    };

    pub const Warning = struct {
        line: u32,
        message: []const u8,
    };

    pub fn init() ValidationResult {
        return .{
            .is_valid = true,
            .estimated_instructions = 0,
            .estimated_memory = 0,
            .error_message = null,
            .line_number = null,
            .features_used = .{},
            .warnings = .{},
        };
    }

    pub fn addWarning(self: *ValidationResult, line: u32, message: []const u8) void {
        self.warnings.append(.{ .line = line, .message = message }) catch {};
    }
};

pub const ValidatorConfig = struct {
    max_instructions: u32 = 10000,
    allowed_features: []const gpu_sandbox.GpuConfig.Feature = &.{.compute},
    max_loop_depth: u32 = 4,
    max_array_size: u32 = 65536,
    max_workgroup_size: u32 = 1024,
    max_buffers: u32 = 8,
    max_textures: u32 = 8,
    allow_derivatives: bool = false, // dpdx/dpdy can cause issues
};

/// Validate a WGSL shader source
pub fn validate(wgsl: []const u8, config: ValidatorConfig) !void {
    var result = analyzeShader(wgsl);

    if (!result.is_valid) {
        std.debug.print("[shader-validator] Invalid shader: {s}\n", .{
            result.error_message orelse "unknown error",
        });
        return ValidationError.InvalidSyntax;
    }

    // Check instruction complexity
    if (result.estimated_instructions > config.max_instructions) {
        std.debug.print("[shader-validator] Shader too complex: {} > {} instructions\n", .{
            result.estimated_instructions,
            config.max_instructions,
        });
        return ValidationError.TooComplex;
    }

    // Check loop depth
    if (result.features_used.max_loop_depth > config.max_loop_depth) {
        std.debug.print("[shader-validator] Excessive loop nesting: {} > {}\n", .{
            result.features_used.max_loop_depth,
            config.max_loop_depth,
        });
        return ValidationError.InfiniteLoop;
    }

    // Check workgroup size
    const total_workgroup = result.features_used.workgroup_size[0] *
        result.features_used.workgroup_size[1] *
        result.features_used.workgroup_size[2];
    if (total_workgroup > config.max_workgroup_size) {
        std.debug.print("[shader-validator] Workgroup size too large: {} > {}\n", .{
            total_workgroup,
            config.max_workgroup_size,
        });
        return ValidationError.InvalidWorkgroupSize;
    }

    // Check buffer/texture counts
    if (result.features_used.buffer_count > config.max_buffers) {
        return ValidationError.ExcessiveMemory;
    }
    if (result.features_used.texture_count > config.max_textures) {
        return ValidationError.ExcessiveMemory;
    }

    // Check derivatives (can cause GPU hangs on some drivers)
    if (result.features_used.uses_derivative and !config.allow_derivatives) {
        return ValidationError.DangerousPattern;
    }

    // Check allowed features
    try checkFeatures(&result.features_used, config.allowed_features);

    // Print warnings
    for (result.warnings.slice()) |warning| {
        std.debug.print("[shader-validator] Warning (line {}): {s}\n", .{ warning.line, warning.message });
    }
}

fn checkFeatures(used: *const ValidationResult.FeatureSet, allowed: []const gpu_sandbox.GpuConfig.Feature) !void {
    var compute_allowed = false;
    var vertex_allowed = false;
    var fragment_allowed = false;
    var storage_allowed = false;

    for (allowed) |feature| {
        switch (feature) {
            .compute => compute_allowed = true,
            .vertex => vertex_allowed = true,
            .fragment => fragment_allowed = true,
            .storage_textures => storage_allowed = true,
        }
    }

    if (used.uses_compute and !compute_allowed) {
        return ValidationError.DisallowedFeature;
    }
    if (used.uses_vertex and !vertex_allowed) {
        return ValidationError.DisallowedFeature;
    }
    if (used.uses_fragment and !fragment_allowed) {
        return ValidationError.DisallowedFeature;
    }
    if (used.uses_storage_textures and !storage_allowed) {
        return ValidationError.DisallowedFeature;
    }
}

/// Analyze WGSL shader and estimate complexity
fn analyzeShader(wgsl: []const u8) ValidationResult {
    var result = ValidationResult.init();

    var line_number: u32 = 1;
    var i: usize = 0;
    var loop_depth: u32 = 0;
    var brace_depth: u32 = 0;
    var in_comment = false;
    var in_block_comment = false;

    while (i < wgsl.len) {
        const c = wgsl[i];

        // Handle newlines
        if (c == '\n') {
            line_number += 1;
            in_comment = false;
            i += 1;
            continue;
        }

        // Handle comments
        if (!in_block_comment and i + 1 < wgsl.len) {
            if (c == '/' and wgsl[i + 1] == '/') {
                in_comment = true;
                i += 2;
                continue;
            }
            if (c == '/' and wgsl[i + 1] == '*') {
                in_block_comment = true;
                i += 2;
                continue;
            }
        }
        if (in_block_comment) {
            if (c == '*' and i + 1 < wgsl.len and wgsl[i + 1] == '/') {
                in_block_comment = false;
                i += 2;
                continue;
            }
            i += 1;
            continue;
        }
        if (in_comment) {
            i += 1;
            continue;
        }

        // Track braces for scope
        if (c == '{') {
            brace_depth += 1;
            i += 1;
            continue;
        }
        if (c == '}') {
            if (brace_depth > 0) {
                brace_depth -= 1;
            } else {
                result.is_valid = false;
                result.error_message = "Unbalanced braces";
                result.line_number = line_number;
                return result;
            }
            if (loop_depth > 0 and brace_depth < loop_depth) loop_depth -= 1;
            i += 1;
            continue;
        }

        // Skip whitespace
        if (std.ascii.isWhitespace(c)) {
            i += 1;
            continue;
        }

        // Check for keywords and patterns
        if (matchKeyword(wgsl[i..])) |keyword| {
            switch (keyword.kind) {
                .compute_stage => result.features_used.uses_compute = true,
                .vertex_stage => result.features_used.uses_vertex = true,
                .fragment_stage => result.features_used.uses_fragment = true,
                .loop_keyword => {
                    loop_depth += 1;
                    result.features_used.loop_count += 1;
                    if (loop_depth > result.features_used.max_loop_depth) {
                        result.features_used.max_loop_depth = loop_depth;
                    }
                    // Warn about unbounded loops
                    if (keyword.len == 4 and std.mem.eql(u8, wgsl[i .. i + 4], "loop")) {
                        result.addWarning(line_number, "Unbounded 'loop' keyword - ensure break condition exists");
                    }
                },
                .storage => result.features_used.uses_storage_textures = true,
                .atomic => {
                    result.features_used.uses_atomics = true;
                    result.estimated_instructions += keyword.cost;
                },
                .texture_sample => {
                    result.features_used.uses_texture_sample = true;
                    result.estimated_instructions += keyword.cost;
                },
                .derivative => {
                    result.features_used.uses_derivative = true;
                    result.estimated_instructions += keyword.cost;
                },
                .buffer_binding => result.features_used.buffer_count += 1,
                .texture_binding => result.features_used.texture_count += 1,
                .workgroup_size => {
                    // Try to parse workgroup_size(x, y, z)
                    if (parseWorkgroupSize(wgsl[i + keyword.len ..])) |size| {
                        result.features_used.workgroup_size = size;
                    }
                },
                .dangerous => {
                    result.addWarning(line_number, keyword.warning orelse "Potentially dangerous pattern");
                    result.estimated_instructions += keyword.cost;
                },
                .instruction => result.estimated_instructions += keyword.cost,
            }
            i += keyword.len;
        } else {
            i += 1;
        }
    }

    // Check for unbalanced braces
    if (brace_depth != 0) {
        result.is_valid = false;
        result.error_message = "Unbalanced braces at end of shader";
        return result;
    }

    // Base cost for shader overhead
    result.estimated_instructions += 100;

    // Estimate memory from buffer/texture bindings
    result.estimated_memory = result.features_used.buffer_count * 4096 +
        result.features_used.texture_count * 16384;

    return result;
}

fn parseWorkgroupSize(text: []const u8) ?[3]u32 {
    // Look for (x) or (x, y) or (x, y, z)
    var i: usize = 0;

    // Skip whitespace
    while (i < text.len and std.ascii.isWhitespace(text[i])) : (i += 1) {}

    if (i >= text.len or text[i] != '(') return null;
    i += 1;

    var sizes: [3]u32 = .{ 1, 1, 1 };
    var dim: usize = 0;

    while (i < text.len and dim < 3) {
        // Skip whitespace
        while (i < text.len and std.ascii.isWhitespace(text[i])) : (i += 1) {}

        // Parse number
        var num: u32 = 0;
        var has_digits = false;
        while (i < text.len and std.ascii.isDigit(text[i])) {
            num = num * 10 + (text[i] - '0');
            has_digits = true;
            i += 1;
        }

        if (!has_digits) break;
        sizes[dim] = num;
        dim += 1;

        // Skip whitespace
        while (i < text.len and std.ascii.isWhitespace(text[i])) : (i += 1) {}

        if (i >= text.len) break;
        if (text[i] == ')') break;
        if (text[i] == ',') {
            i += 1;
        } else {
            break;
        }
    }

    if (dim > 0) return sizes;
    return null;
}

const KeywordMatch = struct {
    len: usize,
    kind: Kind,
    cost: u32 = 1,
    warning: ?[]const u8 = null,

    const Kind = enum {
        compute_stage,
        vertex_stage,
        fragment_stage,
        loop_keyword,
        storage,
        atomic,
        texture_sample,
        derivative,
        buffer_binding,
        texture_binding,
        workgroup_size,
        dangerous,
        instruction,
    };
};

fn matchKeyword(text: []const u8) ?KeywordMatch {
    // Entry point decorators
    if (startsWith(text, "@compute")) return .{ .len = 8, .kind = .compute_stage };
    if (startsWith(text, "@vertex")) return .{ .len = 7, .kind = .vertex_stage };
    if (startsWith(text, "@fragment")) return .{ .len = 9, .kind = .fragment_stage };

    // Workgroup size
    if (startsWith(text, "@workgroup_size")) return .{ .len = 15, .kind = .workgroup_size };

    // Bindings
    if (startsWith(text, "@group")) return .{ .len = 6, .kind = .buffer_binding };
    if (startsWith(text, "var<storage")) return .{ .len = 11, .kind = .buffer_binding };
    if (startsWith(text, "var<uniform")) return .{ .len = 11, .kind = .buffer_binding };

    // Textures
    if (startsWith(text, "texture_2d")) return .{ .len = 10, .kind = .texture_binding };
    if (startsWith(text, "texture_3d")) return .{ .len = 10, .kind = .texture_binding };
    if (startsWith(text, "texture_cube")) return .{ .len = 12, .kind = .texture_binding };
    if (startsWith(text, "texture_storage")) return .{ .len = 15, .kind = .storage };

    // Control flow
    if (startsWithWord(text, "for")) return .{ .len = 3, .kind = .loop_keyword };
    if (startsWithWord(text, "while")) return .{ .len = 5, .kind = .loop_keyword };
    if (startsWithWord(text, "loop")) return .{ .len = 4, .kind = .loop_keyword };

    // Texture sampling (expensive)
    if (startsWith(text, "textureSample")) return .{ .len = 13, .kind = .texture_sample, .cost = 20 };
    if (startsWith(text, "textureLoad")) return .{ .len = 11, .kind = .texture_sample, .cost = 15 };
    if (startsWith(text, "textureStore")) return .{ .len = 12, .kind = .texture_sample, .cost = 15 };

    // Derivatives (can cause issues)
    if (startsWithWord(text, "dpdx")) return .{ .len = 4, .kind = .derivative, .cost = 5 };
    if (startsWithWord(text, "dpdy")) return .{ .len = 4, .kind = .derivative, .cost = 5 };
    if (startsWithWord(text, "fwidth")) return .{ .len = 6, .kind = .derivative, .cost = 10 };

    // Atomics
    if (startsWith(text, "atomicAdd")) return .{ .len = 9, .kind = .atomic, .cost = 10 };
    if (startsWith(text, "atomicSub")) return .{ .len = 9, .kind = .atomic, .cost = 10 };
    if (startsWith(text, "atomicMax")) return .{ .len = 9, .kind = .atomic, .cost = 10 };
    if (startsWith(text, "atomicMin")) return .{ .len = 9, .kind = .atomic, .cost = 10 };
    if (startsWith(text, "atomicAnd")) return .{ .len = 9, .kind = .atomic, .cost = 10 };
    if (startsWith(text, "atomicOr")) return .{ .len = 8, .kind = .atomic, .cost = 10 };
    if (startsWith(text, "atomicXor")) return .{ .len = 9, .kind = .atomic, .cost = 10 };
    if (startsWith(text, "atomicExchange")) return .{ .len = 14, .kind = .atomic, .cost = 15 };
    if (startsWith(text, "atomicCompareExchangeWeak")) return .{ .len = 25, .kind = .atomic, .cost = 20 };

    // Dangerous patterns
    if (startsWith(text, "discard")) return .{
        .len = 7,
        .kind = .dangerous,
        .cost = 50,
        .warning = "discard can cause performance issues",
    };

    // Expensive math
    if (startsWithWord(text, "sin") or startsWithWord(text, "cos") or startsWithWord(text, "tan")) {
        return .{ .len = 3, .kind = .instruction, .cost = 5 };
    }
    if (startsWithWord(text, "asin") or startsWithWord(text, "acos") or startsWithWord(text, "atan")) {
        return .{ .len = 4, .kind = .instruction, .cost = 8 };
    }
    if (startsWithWord(text, "exp") or startsWithWord(text, "log")) {
        return .{ .len = 3, .kind = .instruction, .cost = 8 };
    }
    if (startsWithWord(text, "exp2") or startsWithWord(text, "log2")) {
        return .{ .len = 4, .kind = .instruction, .cost = 6 };
    }
    if (startsWithWord(text, "pow")) return .{ .len = 3, .kind = .instruction, .cost = 10 };
    if (startsWithWord(text, "sqrt")) return .{ .len = 4, .kind = .instruction, .cost = 5 };
    if (startsWithWord(text, "inverseSqrt")) return .{ .len = 11, .kind = .instruction, .cost = 6 };

    // Matrix ops
    if (startsWith(text, "mat4x4")) return .{ .len = 6, .kind = .instruction, .cost = 16 };
    if (startsWith(text, "mat3x3")) return .{ .len = 6, .kind = .instruction, .cost = 9 };
    if (startsWith(text, "mat2x2")) return .{ .len = 6, .kind = .instruction, .cost = 4 };

    // Vector ops
    if (startsWith(text, "vec4")) return .{ .len = 4, .kind = .instruction, .cost = 4 };
    if (startsWith(text, "vec3")) return .{ .len = 4, .kind = .instruction, .cost = 3 };
    if (startsWith(text, "vec2")) return .{ .len = 4, .kind = .instruction, .cost = 2 };

    // Cross/dot/normalize
    if (startsWithWord(text, "cross")) return .{ .len = 5, .kind = .instruction, .cost = 5 };
    if (startsWithWord(text, "dot")) return .{ .len = 3, .kind = .instruction, .cost = 4 };
    if (startsWithWord(text, "normalize")) return .{ .len = 9, .kind = .instruction, .cost = 6 };
    if (startsWithWord(text, "length")) return .{ .len = 6, .kind = .instruction, .cost = 5 };
    if (startsWithWord(text, "distance")) return .{ .len = 8, .kind = .instruction, .cost = 6 };

    return null;
}

fn startsWith(text: []const u8, prefix: []const u8) bool {
    if (text.len < prefix.len) return false;
    return std.mem.eql(u8, text[0..prefix.len], prefix);
}

fn startsWithWord(text: []const u8, word: []const u8) bool {
    if (!startsWith(text, word)) return false;
    if (text.len > word.len) {
        const next = text[word.len];
        return !std.ascii.isAlphanumeric(next) and next != '_';
    }
    return true;
}

// Tests
test "validate simple compute shader" {
    const shader =
        \\@compute @workgroup_size(64)
        \\fn main(@builtin(global_invocation_id) id: vec3<u32>) {
        \\    let x = id.x;
        \\}
    ;

    try validate(shader, .{});
}

test "reject shader with too many loops" {
    const shader =
        \\@compute @workgroup_size(64)
        \\fn main() {
        \\    for (var i = 0; i < 10; i++) {
        \\        for (var j = 0; j < 10; j++) {
        \\            for (var k = 0; k < 10; k++) {
        \\                for (var l = 0; l < 10; l++) {
        \\                    for (var m = 0; m < 10; m++) {
        \\                    }
        \\                }
        \\            }
        \\        }
        \\    }
        \\}
    ;

    const result = validate(shader, .{ .max_loop_depth = 4 });
    try std.testing.expectError(ValidationError.InfiniteLoop, result);
}

test "detect compute feature" {
    const shader =
        \\@compute @workgroup_size(64)
        \\fn main() {}
    ;

    const result = analyzeShader(shader);
    try std.testing.expect(result.features_used.uses_compute);
    try std.testing.expect(!result.features_used.uses_vertex);
}

test "parse workgroup size" {
    const shader =
        \\@compute @workgroup_size(256, 1, 1)
        \\fn main() {}
    ;

    const result = analyzeShader(shader);
    try std.testing.expectEqual(@as(u32, 256), result.features_used.workgroup_size[0]);
    try std.testing.expectEqual(@as(u32, 1), result.features_used.workgroup_size[1]);
}

test "reject oversized workgroup" {
    const shader =
        \\@compute @workgroup_size(2048)
        \\fn main() {}
    ;

    const result = validate(shader, .{ .max_workgroup_size = 1024 });
    try std.testing.expectError(ValidationError.InvalidWorkgroupSize, result);
}

test "detect unbalanced braces" {
    const shader =
        \\@compute @workgroup_size(64)
        \\fn main() {
        \\    let x = 1;
    ;

    const result = analyzeShader(shader);
    try std.testing.expect(!result.is_valid);
}

test "handle comments" {
    const shader =
        \\// This is a comment with loop keyword that should be ignored
        \\/*
        \\   Block comment with for and while
        \\*/
        \\@compute @workgroup_size(64)
        \\fn main() {
        \\    // for loop in comment - ignored
        \\    let x = 1;
        \\}
    ;

    const result = analyzeShader(shader);
    try std.testing.expect(result.is_valid);
    try std.testing.expectEqual(@as(u32, 0), result.features_used.loop_count);
}
