/// External Host Function Dispatch - Single Dispatch Pattern
/// Reduces WASM link time by minimizing import count
const std = @import("std");

// Single dispatch function per module
pub extern "edgebox_socket" fn socket_dispatch(opcode: u32, a1: u32, a2: u32, a3: u32) i32;
pub extern "edgebox_spawn" fn spawn_dispatch(opcode: u32, a1: u32, a2: u32, a3: u32, a4: u32) i32;
pub extern "edgebox_http" fn http_dispatch(opcode: u32, a1: u32, a2: u32, a3: u32, a4: u32, a5: u32, a6: u32, a7: u32, a8: u32) i32;
pub extern "edgebox_file" fn file_dispatch(opcode: u32, a1: u32, a2: u32, a3: u32, a4: u32) i32;
pub extern "edgebox_zlib" fn zlib_dispatch(opcode: u32, a1: u32, a2: u32) i32;
pub extern "edgebox_crypto" fn crypto_dispatch(opcode: u32, a1: u32, a2: u32, a3: u32, a4: u32, a5: u32, a6: u32) i32;
pub extern "edgebox_stdlib" fn stdlib_dispatch(opcode: u32, a1: u32, a2: u32, a3: u32, a4: u32) i32;
pub extern "edgebox_process_cm" fn process_cm_dispatch(opcode: u32, a1: u32, a2: u32, a3: u32, a4: u32, a5: u32, a6: u32, a7: u32) i32;

// ============================================================================
// Socket Opcodes
// ============================================================================
pub const SOCKET_OP_CREATE: u32 = 0;
pub const SOCKET_OP_BIND: u32 = 1;
pub const SOCKET_OP_LISTEN: u32 = 2;
pub const SOCKET_OP_ACCEPT: u32 = 3;
pub const SOCKET_OP_CONNECT: u32 = 4;
pub const SOCKET_OP_WRITE: u32 = 5;
pub const SOCKET_OP_READ: u32 = 6;
pub const SOCKET_OP_GET_READ_DATA: u32 = 7;
pub const SOCKET_OP_CLOSE: u32 = 8;
pub const SOCKET_OP_STATE: u32 = 9;

// ============================================================================
// Spawn Opcodes
// ============================================================================
pub const SPAWN_OP_START: u32 = 0;
pub const SPAWN_OP_POLL: u32 = 1;
pub const SPAWN_OP_OUTPUT_LEN: u32 = 2;
pub const SPAWN_OP_OUTPUT: u32 = 3;
pub const SPAWN_OP_FREE: u32 = 4;

// ============================================================================
// HTTP Opcodes
// ============================================================================
pub const HTTP_OP_REQUEST: u32 = 0;
pub const HTTP_OP_GET_RESPONSE_LEN: u32 = 1;
pub const HTTP_OP_GET_RESPONSE: u32 = 2;
pub const HTTP_OP_START_ASYNC: u32 = 3;
pub const HTTP_OP_POLL: u32 = 4;
pub const HTTP_OP_RESPONSE_LEN: u32 = 5;
pub const HTTP_OP_RESPONSE: u32 = 6;
pub const HTTP_OP_FREE: u32 = 7;

// ============================================================================
// File Opcodes
// ============================================================================
pub const FILE_OP_READ_START: u32 = 0;
pub const FILE_OP_WRITE_START: u32 = 1;
pub const FILE_OP_POLL: u32 = 2;
pub const FILE_OP_RESULT_LEN: u32 = 3;
pub const FILE_OP_RESULT: u32 = 4;
pub const FILE_OP_FREE: u32 = 5;

// ============================================================================
// Zlib Opcodes
// ============================================================================
pub const ZLIB_OP_GZIP: u32 = 0;
pub const ZLIB_OP_GUNZIP: u32 = 1;
pub const ZLIB_OP_DEFLATE: u32 = 2;
pub const ZLIB_OP_INFLATE: u32 = 3;
pub const ZLIB_OP_GET_RESULT: u32 = 4;

// ============================================================================
// Crypto Opcodes (Component Model - Phase 9b)
// ============================================================================
pub const CRYPTO_OP_HASH: u32 = 0;
pub const CRYPTO_OP_HMAC: u32 = 1;
pub const CRYPTO_OP_RANDOM_BYTES: u32 = 2;
pub const CRYPTO_OP_GET_RESULT_LEN: u32 = 3;
pub const CRYPTO_OP_GET_RESULT: u32 = 4;

// ============================================================================
// Process Component Model Opcodes (Phase 9b)
// ============================================================================
pub const PROCESS_CM_SPAWN_SYNC: u32 = 200;
pub const PROCESS_CM_GET_RESULT_LEN: u32 = 201;
pub const PROCESS_CM_GET_RESULT: u32 = 202;

// ============================================================================
// File Component Model Opcodes (Phase 9b)
// Opcodes 100+ to avoid collision with legacy async file ops (0-5)
// ============================================================================
pub const FILE_CM_READ: u32 = 100;
pub const FILE_CM_WRITE: u32 = 101;
pub const FILE_CM_EXISTS: u32 = 102;
pub const FILE_CM_STAT: u32 = 103;
pub const FILE_CM_READDIR: u32 = 104;
pub const FILE_CM_MKDIR: u32 = 105;
pub const FILE_CM_UNLINK: u32 = 106;
pub const FILE_CM_RMDIR: u32 = 107;
pub const FILE_CM_RENAME: u32 = 108;
pub const FILE_CM_COPY: u32 = 109;
pub const FILE_CM_GET_RESULT_LEN: u32 = 110;
pub const FILE_CM_GET_RESULT: u32 = 111;

// ============================================================================
// Stdlib Opcodes (Array: 0-9, Map: 10-19)
// ============================================================================
pub const STDLIB_OP_ARRAY_NEW: u32 = 0;
pub const STDLIB_OP_ARRAY_PUSH: u32 = 1;
pub const STDLIB_OP_ARRAY_POP: u32 = 2;
pub const STDLIB_OP_ARRAY_GET: u32 = 3;
pub const STDLIB_OP_ARRAY_SET: u32 = 4;
pub const STDLIB_OP_ARRAY_LEN: u32 = 5;
pub const STDLIB_OP_ARRAY_SORT: u32 = 6;
pub const STDLIB_OP_ARRAY_FREE: u32 = 7;
pub const STDLIB_OP_ARRAY_SORT_DESC: u32 = 8;
pub const STDLIB_OP_ARRAY_REVERSE: u32 = 9;
pub const STDLIB_OP_MAP_NEW: u32 = 10;
pub const STDLIB_OP_MAP_SET: u32 = 11;
pub const STDLIB_OP_MAP_GET: u32 = 12;
pub const STDLIB_OP_MAP_HAS: u32 = 13;
pub const STDLIB_OP_MAP_DELETE: u32 = 14;
pub const STDLIB_OP_MAP_LEN: u32 = 15;
pub const STDLIB_OP_MAP_FREE: u32 = 16;
pub const STDLIB_OP_MAP_CLEAR: u32 = 17;
pub const STDLIB_OP_ARRAY_CLEAR: u32 = 18;
pub const STDLIB_OP_ARRAY_INDEX_OF: u32 = 19;

// ============================================================================
// Feature Flags for Component Model Migration (Phase 9b)
// ============================================================================
pub const USE_COMPONENT_MODEL_CRYPTO: bool = true;
pub const USE_COMPONENT_MODEL_FS: bool = true;
pub const USE_COMPONENT_MODEL_PROCESS: bool = true;

// ============================================================================
// Socket Wrapper (maintain existing API)
// ============================================================================
pub const socket_host = struct {
    pub fn create() i32 {
        return socket_dispatch(SOCKET_OP_CREATE, 0, 0, 0);
    }
    pub fn bind(socket_id: u32, port: u32) i32 {
        return socket_dispatch(SOCKET_OP_BIND, socket_id, port, 0);
    }
    pub fn listen(socket_id: u32, backlog: u32) i32 {
        return socket_dispatch(SOCKET_OP_LISTEN, socket_id, backlog, 0);
    }
    pub fn accept(socket_id: u32) i32 {
        return socket_dispatch(SOCKET_OP_ACCEPT, socket_id, 0, 0);
    }
    pub fn connect(socket_id: u32, port: u32) i32 {
        return socket_dispatch(SOCKET_OP_CONNECT, socket_id, port, 0);
    }
    pub fn write(socket_id: u32, data_ptr: [*]const u8, data_len: u32) i32 {
        return socket_dispatch(SOCKET_OP_WRITE, socket_id, @intFromPtr(data_ptr), data_len);
    }
    pub fn read(socket_id: u32, max_len: u32) i32 {
        return socket_dispatch(SOCKET_OP_READ, socket_id, max_len, 0);
    }
    pub fn get_read_data(socket_id: u32, dest_ptr: [*]u8) i32 {
        return socket_dispatch(SOCKET_OP_GET_READ_DATA, socket_id, @intFromPtr(dest_ptr), 0);
    }
    pub fn close(socket_id: u32) i32 {
        return socket_dispatch(SOCKET_OP_CLOSE, socket_id, 0, 0);
    }
    pub fn state(socket_id: u32) i32 {
        return socket_dispatch(SOCKET_OP_STATE, socket_id, 0, 0);
    }
};
