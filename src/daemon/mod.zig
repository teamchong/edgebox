//! Daemon Module - EdgeBox Daemon Mode
//!
//! Provides client and server functionality for the EdgeBox daemon:
//!
//! Client (client.zig):
//!   - warmupModule: Pre-load module into cache
//!   - downModule: Unregister module from cache
//!   - exitDaemon: Stop daemon gracefully
//!   - runDaemon: Execute WASM via daemon
//!
//! Server (server.zig):
//!   - runDaemonServer: Main daemon loop
//!   - Module caching with CoW memory
//!
//! Usage:
//!   const daemon = @import("daemon/mod.zig");
//!
//!   // Client operations
//!   try daemon.client.warmupModule(alloc, "app.aot");
//!   try daemon.client.runDaemon(alloc, "app.aot", &.{});
//!
//!   // Server (usually via --daemon-server flag)
//!   try daemon.server.runDaemonServer(...);

pub const client = @import("client.zig");
pub const server = @import("server.zig");

// Re-export common client functions for convenience
pub const warmupModule = client.warmupModule;
pub const downModule = client.downModule;
pub const exitDaemon = client.exitDaemon;
pub const runDaemon = client.runDaemon;
pub const connectToDaemon = client.connectToDaemon;
pub const startDaemonProcess = client.startDaemonProcess;
pub const getSocketPath = client.getSocketPath;
pub const sanitizePath = client.sanitizePath;

// Re-export server functions
pub const runDaemonServer = server.runDaemonServer;
pub const CachedModule = server.CachedModule;
pub const DEFAULT_HEAP_SIZE_MB = server.DEFAULT_HEAP_SIZE_MB;

// Server initialization
pub fn initServer(alloc: @import("std").mem.Allocator) void {
    server.init(alloc);
}
