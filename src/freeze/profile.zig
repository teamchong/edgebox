//! Profiling counters for frozen interpreter performance analysis.
//! Imported by both zig_runtime.zig and js_value.zig (no circular deps).
//!
//! Counter storage and increment function are defined as extern C symbols.
//! The actual storage lives in profile_counters.c (compiled once into the
//! main executable). This ensures ALL frozen shards share the same counters.

const std = @import("std");
const builtin = @import("builtin");

/// Flip to false for zero-overhead production builds.
pub const PROFILE = false;

/// Counter indices for the raw array
const COUNTER_IC_HITS = 0;
const COUNTER_IC_MISSES = 1;
const COUNTER_GETFIELD_CALLS = 2;
const COUNTER_CALL_COUNT = 3;
const COUNTER_CV_TO_JSVALUE = 4;
const COUNTER_JSVALUE_TO_CV = 5;
const COUNTER_REF_DUPS = 6;
const COUNTER_REF_FREES = 7;
const COUNTER_CALL_CYCLES = 8;
const COUNTER_GETFIELD_CYCLES = 9;
const COUNTER_COUNT = 10;

/// Raw counter storage - defined in profile_counters.c (single definition)
extern var g_frozen_profile_counters: [COUNTER_COUNT]u64;

/// Profile counter accessor functions
pub const prof = struct {
    pub fn ic_hits() *u64 {
        return &g_frozen_profile_counters[COUNTER_IC_HITS];
    }
    pub fn ic_misses() *u64 {
        return &g_frozen_profile_counters[COUNTER_IC_MISSES];
    }
    pub fn getfield_calls() *u64 {
        return &g_frozen_profile_counters[COUNTER_GETFIELD_CALLS];
    }
    pub fn call_count() *u64 {
        return &g_frozen_profile_counters[COUNTER_CALL_COUNT];
    }
    pub fn cv_to_jsvalue() *u64 {
        return &g_frozen_profile_counters[COUNTER_CV_TO_JSVALUE];
    }
    pub fn jsvalue_to_cv() *u64 {
        return &g_frozen_profile_counters[COUNTER_JSVALUE_TO_CV];
    }
    pub fn ref_dups() *u64 {
        return &g_frozen_profile_counters[COUNTER_REF_DUPS];
    }
    pub fn ref_frees() *u64 {
        return &g_frozen_profile_counters[COUNTER_REF_FREES];
    }
    pub fn call_cycles() *u64 {
        return &g_frozen_profile_counters[COUNTER_CALL_CYCLES];
    }
    pub fn getfield_cycles() *u64 {
        return &g_frozen_profile_counters[COUNTER_GETFIELD_CYCLES];
    }

    pub fn print() void {
        const counters = &g_frozen_profile_counters;
        std.debug.print("\n=== Frozen Interpreter Profile ===\n", .{});
        std.debug.print("Function calls:    {d}\n", .{counters[COUNTER_CALL_COUNT]});
        std.debug.print("  Call cycles:     {d}\n", .{counters[COUNTER_CALL_CYCLES]});
        std.debug.print("Property access:   {d}\n", .{counters[COUNTER_GETFIELD_CALLS]});
        std.debug.print("  IC hits:         {d} ({d}%)\n", .{
            counters[COUNTER_IC_HITS],
            if (counters[COUNTER_GETFIELD_CALLS] > 0) counters[COUNTER_IC_HITS] * 100 / counters[COUNTER_GETFIELD_CALLS] else 0,
        });
        std.debug.print("  IC misses:       {d}\n", .{counters[COUNTER_IC_MISSES]});
        std.debug.print("  GetField cycles: {d}\n", .{counters[COUNTER_GETFIELD_CYCLES]});
        std.debug.print("CV conversions:    {d} to_jsv + {d} from_jsv\n", .{
            counters[COUNTER_CV_TO_JSVALUE], counters[COUNTER_JSVALUE_TO_CV],
        });
        std.debug.print("Ref counting:      {d} dups + {d} frees\n", .{
            counters[COUNTER_REF_DUPS], counters[COUNTER_REF_FREES],
        });
    }
};

/// Noinline increment — defined in profile_counters.c to ensure single definition.
/// All shards call the same function via extern linkage.
extern fn frozen_profile_vinc(ptr: *u64, delta: u64) void;

/// Wrapper for Zig callers
pub inline fn vinc(ptr: *u64, delta: u64) void {
    frozen_profile_vinc(ptr, delta);
}

pub inline fn cycles() u64 {
    if (comptime builtin.cpu.arch == .aarch64) {
        return asm volatile ("mrs %[val], cntvct_el0"
            : [val] "=r" (-> u64),
        );
    }
    return 0;
}
