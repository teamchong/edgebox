/// WASI-NN Bindings for WasmEdge
/// Provides neural network inference support via WASI-NN specification
///
/// Requires WasmEdge with WASI-NN GGML plugin:
///   curl -sSf https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh | bash -s -- --plugins wasi_nn-ggml
///
/// Usage:
///   wasmedge --nn-preload default:GGML:AUTO:model.gguf your-app.wasm
const std = @import("std");

/// Error codes returned by WASI-NN functions
pub const NnErrno = enum(u16) {
    success = 0,
    invalid_argument = 1,
    invalid_encoding = 2,
    missing_memory = 3,
    busy = 4,
    runtime_error = 5,
    unsupported_operation = 6,
    too_large = 7,
    not_found = 8,
};

/// Graph encoding formats
pub const GraphEncoding = enum(u8) {
    openvino = 0,
    onnx = 1,
    tensorflow = 2,
    pytorch = 3,
    tensorflowlite = 4,
    autodetect = 5,
    ggml = 6, // For llama.cpp models
};

/// Execution target (CPU, GPU, etc.)
pub const ExecutionTarget = enum(u8) {
    cpu = 0,
    gpu = 1,
    tpu = 2,
    auto = 3,
};

/// Tensor data types
pub const TensorType = enum(u8) {
    f16 = 0,
    f32 = 1,
    f64 = 2,
    u8 = 3,
    i32 = 4,
    i64 = 5,
};

/// Graph builder array element (model bytes pointer + length)
pub const GraphBuilderArray = extern struct {
    buf: [*]const u8,
    buf_len: usize,
};

/// Tensor structure for input/output
pub const Tensor = extern struct {
    dimensions: [*]const u32,
    dimensions_len: u32,
    tensor_type: TensorType,
    data: [*]const u8,
    data_len: usize,
};

/// Graph handle (opaque)
pub const Graph = u32;

/// Graph execution context handle (opaque)
pub const GraphExecutionContext = u32;

// WASI-NN imports (from wasi_ephemeral_nn module)
extern "wasi_ephemeral_nn" fn load(
    builder_ptr: [*]const GraphBuilderArray,
    builder_len: u32,
    encoding: GraphEncoding,
    target: ExecutionTarget,
    graph: *Graph,
) NnErrno;

extern "wasi_ephemeral_nn" fn load_by_name(
    name_ptr: [*]const u8,
    name_len: u32,
    graph: *Graph,
) NnErrno;

extern "wasi_ephemeral_nn" fn init_execution_context(
    graph: Graph,
    context: *GraphExecutionContext,
) NnErrno;

extern "wasi_ephemeral_nn" fn set_input(
    context: GraphExecutionContext,
    index: u32,
    tensor: *const Tensor,
) NnErrno;

extern "wasi_ephemeral_nn" fn get_output(
    context: GraphExecutionContext,
    index: u32,
    out_buf: [*]u8,
    out_buf_max: u32,
    bytes_written: *u32,
) NnErrno;

extern "wasi_ephemeral_nn" fn compute(
    context: GraphExecutionContext,
) NnErrno;

// High-level API

/// Load a model by name (from --nn-preload cache)
pub fn loadByName(name: []const u8) !Graph {
    var graph: Graph = undefined;
    const err = load_by_name(name.ptr, @intCast(name.len), &graph);
    if (err != .success) {
        return error.LoadFailed;
    }
    return graph;
}

/// Load a model from bytes
pub fn loadFromBytes(
    model_bytes: []const u8,
    encoding: GraphEncoding,
    target: ExecutionTarget,
) !Graph {
    var builder = GraphBuilderArray{
        .buf = model_bytes.ptr,
        .buf_len = model_bytes.len,
    };
    var graph: Graph = undefined;
    const err = load(&builder, 1, encoding, target, &graph);
    if (err != .success) {
        return error.LoadFailed;
    }
    return graph;
}

/// Initialize an execution context for a graph
pub fn initContext(graph: Graph) !GraphExecutionContext {
    var ctx: GraphExecutionContext = undefined;
    const err = init_execution_context(graph, &ctx);
    if (err != .success) {
        return error.InitContextFailed;
    }
    return ctx;
}

/// Set input tensor
pub fn setInput(ctx: GraphExecutionContext, index: u32, data: []const u8) !void {
    var dims = [_]u32{1};
    var tensor = Tensor{
        .dimensions = &dims,
        .dimensions_len = 1,
        .tensor_type = .u8,
        .data = data.ptr,
        .data_len = data.len,
    };
    const err = set_input(ctx, index, &tensor);
    if (err != .success) {
        return error.SetInputFailed;
    }
}

/// Run inference
pub fn runCompute(ctx: GraphExecutionContext) !void {
    const err = compute(ctx);
    if (err != .success) {
        return error.ComputeFailed;
    }
}

/// Get output tensor (returns bytes written)
pub fn getOutput(ctx: GraphExecutionContext, index: u32, buf: []u8) !usize {
    var bytes_written: u32 = undefined;
    const err = get_output(ctx, index, buf.ptr, @intCast(buf.len), &bytes_written);
    if (err != .success) {
        return error.GetOutputFailed;
    }
    return bytes_written;
}

/// High-level: Run chat completion (for LLM models)
/// Returns generated text in the provided buffer
pub fn chat(prompt: []const u8, output_buf: []u8) ![]u8 {
    // Load model from default preloaded name
    const graph = try loadByName("default");
    const ctx = try initContext(graph);

    // Set the prompt as input
    try setInput(ctx, 0, prompt);

    // Run inference
    try runCompute(ctx);

    // Get output
    const len = try getOutput(ctx, 0, output_buf);
    return output_buf[0..len];
}

/// Check if WASI-NN is available by trying to load a model
pub fn isAvailable() bool {
    var graph: Graph = undefined;
    const err = load_by_name("default", 7, &graph);
    // If we get not_found, WASI-NN is available but no model loaded
    // If we get success, model is loaded
    // Other errors mean WASI-NN isn't available
    return err == .success or err == .not_found;
}
