//! Parallel TSC Type Checking via Zig Goroutines + Go-style Channels
//!
//! Architecture:
//!   N goroutines, each with its own V8 isolate + TSC instance.
//!   Main goroutine parses config, dispatches file shards via Channel.
//!   Worker goroutines check assigned files, send diagnostics back via Channel.
//!   Zig file cache shared across all goroutines (same process, zero-copy).
//!
//! This is the Zig equivalent of tsgo's goroutine-based parallel checker.
//! Unlike workerd (separate isolates via HTTP RPC), this uses direct
//! in-process communication with zero serialization overhead.

const std = @import("std");
const channel = @import("async/channel.zig");

const alloc = std.heap.page_allocator;

/// A batch of diagnostics from one worker
pub const DiagBatch = struct {
    worker_id: u32,
    file_count: u32,
    diag_count: u32,
    /// Pointer to diagnostic data in shared memory (zero-copy)
    data_ptr: ?[*]const u8,
    data_len: u32,
};

/// Shared state for parallel checking
pub const ParallelChecker = struct {
    worker_count: u32,
    /// Channel for workers to send results back to main
    results: *channel.Channel(DiagBatch),
    /// Channel for main to send file assignments to workers
    assignments: *channel.Channel(FileAssignment),

    pub fn init(worker_count: u32) !*ParallelChecker {
        const pc = try alloc.create(ParallelChecker);
        pc.* = .{
            .worker_count = worker_count,
            .results = try channel.makeBuffered(DiagBatch, alloc, worker_count),
            .assignments = try channel.makeBuffered(FileAssignment, alloc, worker_count * 2),
        };
        return pc;
    }

    pub fn deinit(self: *ParallelChecker) void {
        self.results.deinit();
        self.assignments.deinit();
        alloc.destroy(self);
    }

    /// Main thread: dispatch file shards to workers, collect results
    pub fn checkParallel(
        self: *ParallelChecker,
        file_names: []const []const u8,
        config_json: []const u8,
    ) ![]DiagBatch {
        // Send assignments to workers
        const files_per_worker = file_names.len / self.worker_count;
        var i: u32 = 0;
        while (i < self.worker_count) : (i += 1) {
            const start = i * @as(u32, @intCast(files_per_worker));
            const end = if (i == self.worker_count - 1)
                @as(u32, @intCast(file_names.len))
            else
                (i + 1) * @as(u32, @intCast(files_per_worker));

            _ = self.assignments.send(.{
                .worker_id = i,
                .start_idx = start,
                .end_idx = end,
                .config_json = config_json,
            });
        }

        // Collect results from all workers
        var results = try alloc.alloc(DiagBatch, self.worker_count);
        var received: u32 = 0;
        while (received < self.worker_count) : (received += 1) {
            if (self.results.recv()) |batch| {
                results[received] = batch;
            }
        }

        return results;
    }
};

pub const FileAssignment = struct {
    worker_id: u32,
    start_idx: u32,
    end_idx: u32,
    config_json: []const u8,
};

/// Worker function: runs in a goroutine, creates V8 isolate + TSC,
/// checks assigned files, sends results via channel.
pub fn workerFn(checker: *ParallelChecker) void {
    while (true) {
        const assignment = checker.assignments.recv() orelse break;

        // TODO: Create V8 isolate, load TSC, parse program, check files
        // For now, send dummy result
        _ = checker.results.send(.{
            .worker_id = assignment.worker_id,
            .file_count = assignment.end_idx - assignment.start_idx,
            .diag_count = 0,
            .data_ptr = null,
            .data_len = 0,
        });
    }
}
