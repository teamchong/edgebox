//! IPC Module - Inter-Process Communication
//!
//! Provides shared memory IPC for high-performance daemon communication.
//!
//! Usage:
//!   const ipc = @import("ipc/mod.zig");
//!
//!   // Server
//!   var ring = try ipc.SharedRing.open(true);
//!   ring.markServerReady();
//!
//!   // Client
//!   var ring = try ipc.SharedRing.open(false);
//!   const req_id = try ring.sendRequest(path, args);
//!   const resp = try ring.waitResponse(req_id, 30000);

pub const shared_ring = @import("shared_ring.zig");

// Re-export main types
pub const SharedRing = shared_ring.SharedRing;
pub const Request = shared_ring.Request;
pub const Response = shared_ring.Response;
pub const EntryState = shared_ring.EntryState;
pub const Header = shared_ring.Header;

// Constants
pub const MAGIC = shared_ring.MAGIC;
pub const VERSION = shared_ring.VERSION;
pub const TOTAL_SIZE = shared_ring.TOTAL_SIZE;
