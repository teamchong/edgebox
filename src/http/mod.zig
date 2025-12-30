//! Native HTTP Server Module
//! High-performance HTTP server with kqueue/epoll event loop

pub const parser = @import("parser.zig");
pub const event_loop = @import("event_loop.zig");
pub const native_server = @import("native_server.zig");

pub const NativeHttpServer = native_server.NativeHttpServer;
pub const EventLoop = event_loop.EventLoop;
pub const ParsedRequest = parser.ParsedRequest;
pub const Header = parser.Header;
pub const Method = parser.Method;

// Re-export test handler for benchmarking
pub const testHandler = native_server.testHandler;
