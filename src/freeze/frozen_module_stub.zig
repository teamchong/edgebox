//! Stub module for LTO-optimized frozen module
//! When using an externally compiled frozen_module_opt.o, this stub
//! provides the type declarations needed for native_main_static.zig
//!
//! The actual implementation comes from frozen_module_opt.o at link time.

const zig_runtime = @import("zig_runtime");

// Declare frozen_init_c as external - it's defined in the LTO object file
pub extern fn frozen_init_c(ctx: *zig_runtime.JSContext) void;
