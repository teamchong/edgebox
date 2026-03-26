// shared_ast.zig — Live AST in Zig shared memory.
//
// MonoNode constructor writes to Zig on creation (kind, pos, end).
// Property setters write child relationships as they're set by TSC's parser.
// Other workers read directly — no flush, no serialization.
// Same pattern as Go goroutines writing to shared heap.
//
// Memory layout:
//   Node pool:  [FlatNode × MAX_NODES]  — 32 bytes each, indexed by _id
//   Child pool: [u32 × MAX_CHILDREN]    — child node IDs, packed arrays
//   String pool: [u8 × MAX_STRINGS]     — identifier text (escapedText)
//   File slots: [FileSlot × MAX_FILES]  — per-file claim/status

const std = @import("std");
const alloc = std.heap.page_allocator;

pub const NONE: u32 = 0xFFFFFFFF;

// ── Flat Node (40 bytes) ──
pub const FlatNode = extern struct {
    kind: u16,
    _pad: u16 = 0, // alignment padding
    flags: u32, // TSC NodeFlags uses 32 bits (ambient=0x400000, etc.)
    pos: u32,
    end: u32,
    parent: u32,
    text_offset: u32, // escapedText or text string
    text_len: u32,
    modifier_flags: u32,
    transform_flags: u32,
    operator: u16, // TypeOperator, PrefixUnaryExpression etc.
    token: u16, // Token kinds
};

// ── Pool sizes ──
const MAX_NODES: u32 = 8 * 1024 * 1024; // 8M nodes
const MAX_TRIPLES: u32 = 16 * 1024 * 1024; // 16M triples
const MAX_STRINGS: u32 = 128 * 1024 * 1024; // 128MB string table (JSON own properties)
const MAX_FILES: u32 = 2048;

const FILE_UNCLAIMED: u32 = 0;
const FILE_PARSING: u32 = 1;
const FILE_READY: u32 = 2;

const MAX_BLOBS: u32 = 256 * 1024 * 1024; // 256MB for V8 serialized AST blobs

// ── Global pools (shared across all workers) ──
var node_pool: []FlatNode = &.{};
var triple_pool: []u32 = &.{}; // (file_hash, parent_id, prop_id, child_id) × N
var string_pool: []u8 = &.{};
var blob_pool: []u8 = &.{}; // V8-serialized AST blobs

var next_node: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
var next_triple: std.atomic.Value(u32) = std.atomic.Value(u32).init(0); // index in u32s (3 per triple)
var next_string: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
var next_blob: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);

fn ensurePools() void {
    if (node_pool.len > 0) return;
    node_pool = alloc.alloc(FlatNode, MAX_NODES) catch return;
    triple_pool = alloc.alloc(u32, MAX_TRIPLES * 4) catch return; // 4 u32s per quad (file_hash, parent, prop, child)
    string_pool = alloc.alloc(u8, MAX_STRINGS) catch return;
    blob_pool = alloc.alloc(u8, MAX_BLOBS) catch return;
    @memset(std.mem.sliceAsBytes(node_pool), 0);
}

// ── File slots ──
const FileSlot = struct {
    path_hash: u64 = 0,
    status: std.atomic.Value(u32) = std.atomic.Value(u32).init(FILE_UNCLAIMED),
    owner: u32 = 0,
    root_node: u32 = NONE,
    triple_start: u32 = 0, // start index in triple_pool (u32 index, not triple index)
    triple_end: u32 = 0, // end index in triple_pool
    blob_offset: u32 = NONE, // V8-serialized AST blob offset in blob_pool
    blob_len: u32 = 0, // V8-serialized AST blob length
};

var file_slots: [MAX_FILES]FileSlot = [_]FileSlot{.{}} ** MAX_FILES;
var file_slot_count: std.atomic.Value(u32) = std.atomic.Value(u32).init(0);
var file_slot_mutex: std.Thread.Mutex = .{};

fn hashPath(path: []const u8) u64 {
    var h: u64 = 14695981039346656037;
    for (path) |c| {
        h ^= @as(u64, c);
        h *%= 1099511628211;
    }
    return h;
}

fn findOrCreateSlot(path_hash: u64) ?*FileSlot {
    const count = file_slot_count.load(.acquire);
    for (0..count) |i| {
        if (file_slots[i].path_hash == path_hash) return &file_slots[i];
    }
    file_slot_mutex.lock();
    defer file_slot_mutex.unlock();
    const count2 = file_slot_count.load(.acquire);
    for (0..count2) |i| {
        if (file_slots[i].path_hash == path_hash) return &file_slots[i];
    }
    if (count2 >= MAX_FILES) return null;
    file_slots[count2].path_hash = path_hash;
    file_slot_count.store(count2 + 1, .release);
    return &file_slots[count2];
}

// ── C ABI: Node allocation (called from MonoNode constructor) ──

/// Allocate a node slot. Returns _id. Called for every new MonoNode.
export fn shared_ast_alloc_node(kind: u16, pos: u32, end: u32) u32 {
    ensurePools();
    const id = next_node.fetchAdd(1, .monotonic);
    if (id >= MAX_NODES) return NONE;
    node_pool[id] = .{
        .kind = kind,
        .flags = 0,
        .pos = pos,
        .end = end,
        .parent = NONE,
        .text_offset = NONE,
        .text_len = 0,
        .modifier_flags = 0,
        .transform_flags = 0,
        .operator = 0,
        .token = 0,
    };
    return id;
}

/// Set node flags (called when parser sets node.flags).
export fn shared_ast_set_flags(id: u32, flags: u32) void {
    if (id < MAX_NODES and node_pool.len > 0) {
        node_pool[id].flags = flags;
    }
}

/// Set modifier + transform flags.
export fn shared_ast_set_extra_flags(id: u32, modifier: u32, transform: u32) void {
    if (id < MAX_NODES and node_pool.len > 0) {
        node_pool[id].modifier_flags = modifier;
        node_pool[id].transform_flags = transform;
    }
}

/// Set operator + token fields.
export fn shared_ast_set_op_token(id: u32, operator: u16, token: u16) void {
    if (id < MAX_NODES and node_pool.len > 0) {
        node_pool[id].operator = operator;
        node_pool[id].token = token;
    }
}

/// Set parent (called when parser sets node.parent).
export fn shared_ast_set_parent(id: u32, parent_id: u32) void {
    if (id < MAX_NODES and node_pool.len > 0) {
        node_pool[id].parent = parent_id;
    }
}

/// Store node text/scalars. Returns 1 on success, 0 on pool overflow.
export fn shared_ast_set_text(id: u32, text_ptr: [*]const u8, text_len: c_int) c_int {
    if (id >= MAX_NODES or node_pool.len == 0 or string_pool.len == 0 or text_len <= 0) return 0;
    const len: u32 = @intCast(text_len);
    const offset = next_string.fetchAdd(len, .monotonic);
    if (offset + len > MAX_STRINGS) return 0; // overflow
    @memcpy(string_pool[offset .. offset + len], text_ptr[0..len]);
    node_pool[id].text_offset = offset;
    node_pool[id].text_len = len;
    return 1;
}

/// Append child quads (file_hash_lo, parent_id, prop_id, child_id) for each child.
/// file_hash_lo identifies which file owns this triple (filters interleaved data).
export fn shared_ast_set_children(id: u32, prop_id: u16, children_ptr: [*]const u32, count: c_int, file_hash: u32) void {
    if (id >= MAX_NODES or node_pool.len == 0 or count <= 0) return;
    const n: u32 = @intCast(count);
    const start = next_triple.fetchAdd(n * 4, .monotonic);
    if (start + n * 4 > MAX_TRIPLES * 4) return; // overflow guard
    for (0..n) |i| {
        triple_pool[start + i * 4 + 0] = file_hash; // file owner
        triple_pool[start + i * 4 + 1] = id; // parent
        triple_pool[start + i * 4 + 2] = @as(u32, prop_id); // prop
        triple_pool[start + i * 4 + 3] = children_ptr[i]; // child
        if (children_ptr[i] < MAX_NODES) {
            node_pool[children_ptr[i]].parent = id;
        }
    }
}

// ── C ABI: Node reading (called from reconstruction) ──

/// Get node data. Returns pointer to FlatNode (read-only after READY).
export fn shared_ast_get_node(id: u32) ?*const FlatNode {
    if (id >= MAX_NODES or node_pool.len == 0) return null;
    return &node_pool[id];
}

/// Get total u32 count in triple pool (for bulk read).
export fn shared_ast_get_triple_count() u32 {
    return next_triple.load(.acquire);
}

/// Get pointer to triple pool (for bulk read from C++).
export fn shared_ast_get_triple_pool() ?[*]const u32 {
    if (triple_pool.len == 0) return null;
    return triple_pool.ptr;
}

/// Get text for a node.
export fn shared_ast_get_text(id: u32, out_ptr: *?[*]const u8, out_len: *u32) void {
    if (id >= MAX_NODES or node_pool.len == 0) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    const node = &node_pool[id];
    if (node.text_offset == NONE or node.text_len == 0) {
        out_ptr.* = null;
        out_len.* = 0;
        return;
    }
    out_ptr.* = string_pool[node.text_offset..].ptr;
    out_len.* = node.text_len;
}

// ── C ABI: File claim ──

/// Claim a file. Returns 0=claimed, 1=another worker parsing, 2=ready.
export fn shared_ast_claim(path_ptr: [*]const u8, path_len: c_int) c_int {
    if (path_len <= 0) return 0;
    ensurePools();
    const path = path_ptr[0..@intCast(path_len)];
    const h = hashPath(path);
    const slot = findOrCreateSlot(h) orelse return 0;
    const prev = slot.status.cmpxchgStrong(FILE_UNCLAIMED, FILE_PARSING, .acq_rel, .acquire);
    if (prev == null) return 0;
    if (prev.? == FILE_READY) return 2;
    return 1;
}

/// Wait for a file to become READY. Blocks on condvar until ready.
/// Split roots ensures no circular deps — workers process different root files.
export fn shared_ast_wait_file(path_ptr: [*]const u8, path_len: c_int) void {
    if (path_len <= 0) return;
    const path = path_ptr[0..@intCast(path_len)];
    const h = hashPath(path);
    const count = file_slot_count.load(.acquire);
    for (0..count) |i| {
        if (file_slots[i].path_hash == h) {
            // Quick check without locking
            if (file_slots[i].status.load(.acquire) == FILE_READY) return;
            // Block until ready
            while (file_slots[i].status.load(.acquire) != FILE_READY) {
                file_slot_mutex.lock();
                if (file_slots[i].status.load(.acquire) == FILE_READY) {
                    file_slot_mutex.unlock();
                    return;
                }
                file_ready_cond.timedWait(&file_slot_mutex, 1 * std.time.ns_per_ms) catch {};
                file_slot_mutex.unlock();
            }
            return;
        }
    }
}

// Condvar for file-ready signaling
var file_ready_cond: std.Thread.Condition = .{};

/// Mark file as ready. Sets root node ID + triple range. Wakes all waiting workers.
export fn shared_ast_mark_ready(path_ptr: [*]const u8, path_len: c_int, worker_id: u32, root_node_id: u32, t_start: u32, t_end: u32) void {
    if (path_len <= 0) return;
    const path = path_ptr[0..@intCast(path_len)];
    const h = hashPath(path);
    const slot = findOrCreateSlot(h) orelse return;
    slot.owner = worker_id;
    slot.root_node = root_node_id;
    slot.triple_start = t_start;
    slot.triple_end = t_end;
    slot.status.store(FILE_READY, .release);
    // Wake all workers waiting for any file
    file_ready_cond.broadcast();
}

/// Get file info: root node ID + triple range. Returns NONE root if not ready.
export fn shared_ast_get_root(path_ptr: [*]const u8, path_len: c_int) u32 {
    if (path_len <= 0) return NONE;
    const path = path_ptr[0..@intCast(path_len)];
    const h = hashPath(path);
    const count = file_slot_count.load(.acquire);
    for (0..count) |i| {
        if (file_slots[i].path_hash == h and file_slots[i].status.load(.acquire) == FILE_READY) {
            return file_slots[i].root_node;
        }
    }
    return NONE;
}

/// Get triple range for a file.
export fn shared_ast_get_triple_range(path_ptr: [*]const u8, path_len: c_int, out_start: *u32, out_end: *u32) void {
    if (path_len <= 0) { out_start.* = 0; out_end.* = 0; return; }
    const path = path_ptr[0..@intCast(path_len)];
    const h = hashPath(path);
    const count = file_slot_count.load(.acquire);
    for (0..count) |i| {
        if (file_slots[i].path_hash == h and file_slots[i].status.load(.acquire) == FILE_READY) {
            out_start.* = file_slots[i].triple_start;
            out_end.* = file_slots[i].triple_end;
            return;
        }
    }
    out_start.* = 0;
    out_end.* = 0;
}

/// Get current triple pool write position (for recording range start before link).
export fn shared_ast_get_triple_pos() u32 {
    return next_triple.load(.monotonic);
}

// ── V8 Serialized Blob Storage ──

/// Store a V8-serialized blob. Returns offset in blob_pool, or NONE on overflow.
export fn shared_ast_store_blob(path_ptr: [*]const u8, path_len: c_int, data_ptr: [*]const u8, data_len: u32) u32 {
    if (path_len <= 0 or data_len == 0 or blob_pool.len == 0) return NONE;
    const offset = next_blob.fetchAdd(data_len, .monotonic);
    if (offset + data_len > MAX_BLOBS) return NONE;
    @memcpy(blob_pool[offset .. offset + data_len], data_ptr[0..data_len]);
    // Store offset+len in file slot
    const path = path_ptr[0..@intCast(path_len)];
    const h = hashPath(path);
    const slot = findOrCreateSlot(h) orelse return NONE;
    slot.blob_offset = offset;
    slot.blob_len = data_len;
    return offset;
}

/// Read a V8-serialized blob for a file. Returns pointer+length via out params.
export fn shared_ast_get_blob(path_ptr: [*]const u8, path_len: c_int, out_ptr: *?[*]const u8, out_len: *u32) void {
    if (path_len <= 0 or blob_pool.len == 0) { out_ptr.* = null; out_len.* = 0; return; }
    const path = path_ptr[0..@intCast(path_len)];
    const h = hashPath(path);
    const count = file_slot_count.load(.acquire);
    for (0..count) |i| {
        if (file_slots[i].path_hash == h and file_slots[i].status.load(.acquire) == FILE_READY) {
            if (file_slots[i].blob_offset != NONE and file_slots[i].blob_len > 0) {
                out_ptr.* = blob_pool[file_slots[i].blob_offset..].ptr;
                out_len.* = file_slots[i].blob_len;
                return;
            }
        }
    }
    out_ptr.* = null;
    out_len.* = 0;
}

// ── Reset ──

pub fn resetAll() void {
    for (0..file_slot_count.load(.acquire)) |i| {
        file_slots[i] = .{};
    }
    file_slot_count.store(0, .release);
    next_node.store(0, .release);
    next_triple.store(0, .release);
    next_string.store(0, .release);
    next_blob.store(0, .release);
}

export fn shared_ast_reset() void {
    resetAll();
}
