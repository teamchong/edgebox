// Git Command Emulator
//
// Provides mock implementations of common git commands for sandboxed execution.
// Returns deterministic output without accessing real git repositories.
//
// Supported subcommands:
// - status: Clean working tree
// - branch: List of branches with current marked
// - log: Mock commit history
// - remote: Mock remote URLs
// - rev-parse: Mock commit hash
// - diff: Empty diff
// - config: Mock config values

const std = @import("std");
const EmulatorResult = @import("mod.zig").EmulatorResult;
const OutputMode = @import("mod.zig").OutputMode;

// ============================================================================
// Mock Data
// ============================================================================

const mock_commit_hash = "a1b2c3d4e5f6789012345678901234567890abcd";
const mock_short_hash = "a1b2c3d";
const mock_branch_name = "main";
const mock_remote_url = "https://github.com/example/repo.git";
const mock_author = "EdgeBox Emulator <emulator@edgebox.dev>";
const mock_date = "2025-01-01 00:00:00 +0000";

// ============================================================================
// Main Entry Point
// ============================================================================

pub fn emulate(args: []const []const u8, mode: OutputMode, allocator: std.mem.Allocator) ?EmulatorResult {
    _ = allocator;

    if (args.len == 0) {
        return EmulatorResult.failure("usage: git <command> [<args>]\n", 1);
    }

    const subcommand = args[0];

    if (std.mem.eql(u8, subcommand, "status")) {
        return emulateStatus(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "branch")) {
        return emulateBranch(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "log")) {
        return emulateLog(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "remote")) {
        return emulateRemote(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "rev-parse")) {
        return emulateRevParse(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "diff")) {
        return emulateDiff(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "config")) {
        return emulateConfig(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "show")) {
        return emulateShow(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "ls-files")) {
        return emulateLsFiles(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "--version")) {
        return emulateVersion(mode);
    }

    // Unknown subcommand - return null to fall through to real execution
    return null;
}

// ============================================================================
// Subcommand Implementations
// ============================================================================

fn emulateStatus(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = args;

    const text_output =
        \\On branch main
        \\Your branch is up to date with 'origin/main'.
        \\
        \\nothing to commit, working tree clean
        \\
    ;

    const html_output =
        \\<div class="git-status">
        \\  <p><strong>Branch:</strong> main</p>
        \\  <p class="success">Working tree clean</p>
        \\</div>
    ;

    const viewkit_output =
        \\{"type":"status","branch":"main","clean":true,"ahead":0,"behind":0}
    ;

    return EmulatorResult.success(switch (mode) {
        .server => text_output,
        .browser => html_output,
        .viewkit => viewkit_output,
    });
}

fn emulateBranch(args: []const []const u8, mode: OutputMode) EmulatorResult {
    // Check for -a (all branches) or -r (remote branches)
    var show_remote = false;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "-a") or std.mem.eql(u8, arg, "-r")) {
            show_remote = true;
        }
    }

    const local_branches =
        \\* main
        \\  develop
        \\  feature/test
        \\
    ;

    const all_branches =
        \\* main
        \\  develop
        \\  feature/test
        \\  remotes/origin/main
        \\  remotes/origin/develop
        \\
    ;

    const html_branches =
        \\<ul class="git-branches">
        \\  <li class="current">main</li>
        \\  <li>develop</li>
        \\  <li>feature/test</li>
        \\</ul>
    ;

    const viewkit_branches =
        \\{"type":"branches","current":"main","branches":["main","develop","feature/test"]}
    ;

    return EmulatorResult.success(switch (mode) {
        .server => if (show_remote) all_branches else local_branches,
        .browser => html_branches,
        .viewkit => viewkit_branches,
    });
}

fn emulateLog(args: []const []const u8, mode: OutputMode) EmulatorResult {
    // Check for --oneline flag (limit is parsed but not currently used)
    var oneline = false;

    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--oneline")) {
            oneline = true;
            break;
        }
    }
    // TODO: Use limit parameter when implementing dynamic log responses

    if (oneline) {
        const oneline_output =
            \\a1b2c3d Initial commit
            \\b2c3d4e Add feature
            \\c3d4e5f Fix bug
            \\
        ;
        return EmulatorResult.success(oneline_output);
    }

    const text_output =
        \\commit a1b2c3d4e5f6789012345678901234567890abcd
        \\Author: EdgeBox Emulator <emulator@edgebox.dev>
        \\Date:   Wed Jan 1 00:00:00 2025 +0000
        \\
        \\    Initial commit
        \\
        \\commit b2c3d4e5f6789012345678901234567890abcde
        \\Author: EdgeBox Emulator <emulator@edgebox.dev>
        \\Date:   Wed Jan 1 00:00:00 2025 +0000
        \\
        \\    Add feature
        \\
    ;

    const html_output =
        \\<div class="git-log">
        \\  <div class="commit">
        \\    <span class="hash">a1b2c3d</span>
        \\    <span class="message">Initial commit</span>
        \\    <span class="author">EdgeBox Emulator</span>
        \\  </div>
        \\</div>
    ;

    const viewkit_output =
        \\{"type":"log","commits":[{"hash":"a1b2c3d","message":"Initial commit","author":"EdgeBox Emulator"}]}
    ;

    return EmulatorResult.success(switch (mode) {
        .server => text_output,
        .browser => html_output,
        .viewkit => viewkit_output,
    });
}

fn emulateRemote(args: []const []const u8, mode: OutputMode) EmulatorResult {
    // Check for -v (verbose)
    var verbose = false;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "-v")) {
            verbose = true;
        }
    }

    if (verbose) {
        const verbose_output =
            \\origin  https://github.com/example/repo.git (fetch)
            \\origin  https://github.com/example/repo.git (push)
            \\
        ;
        return EmulatorResult.success(switch (mode) {
            .server => verbose_output,
            .browser => "<pre>" ++ verbose_output ++ "</pre>",
            .viewkit =>
            \\{"type":"remotes","remotes":[{"name":"origin","url":"https://github.com/example/repo.git"}]}
            ,
        });
    }

    return EmulatorResult.success("origin\n");
}

fn emulateRevParse(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = mode;

    for (args) |arg| {
        if (std.mem.eql(u8, arg, "HEAD")) {
            return EmulatorResult.success(mock_commit_hash ++ "\n");
        } else if (std.mem.eql(u8, arg, "--short")) {
            // Next arg should be HEAD
            continue;
        } else if (std.mem.eql(u8, arg, "--abbrev-ref")) {
            // Return branch name
            return EmulatorResult.success(mock_branch_name ++ "\n");
        } else if (std.mem.eql(u8, arg, "--show-toplevel")) {
            return EmulatorResult.success("/mock/repo\n");
        }
    }

    // Check if short was requested
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--short")) {
            return EmulatorResult.success(mock_short_hash ++ "\n");
        }
    }

    return EmulatorResult.success(mock_commit_hash ++ "\n");
}

fn emulateDiff(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = args;
    _ = mode;

    // Return empty diff (no changes)
    return EmulatorResult.success("");
}

fn emulateConfig(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = mode;

    if (args.len == 0) {
        return EmulatorResult.failure("usage: git config <name>\n", 1);
    }

    // Check for --get
    var key: ?[]const u8 = null;
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--get")) {
            continue;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            key = arg;
        }
    }

    if (key == null and args.len > 0) {
        key = args[args.len - 1];
    }

    if (key) |k| {
        if (std.mem.eql(u8, k, "user.name")) {
            return EmulatorResult.success("EdgeBox Emulator\n");
        } else if (std.mem.eql(u8, k, "user.email")) {
            return EmulatorResult.success("emulator@edgebox.dev\n");
        } else if (std.mem.eql(u8, k, "remote.origin.url")) {
            return EmulatorResult.success(mock_remote_url ++ "\n");
        }
    }

    return EmulatorResult.failure("", 1); // Config key not found
}

fn emulateShow(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = mode;

    // Check what we're showing
    for (args) |arg| {
        if (std.mem.indexOf(u8, arg, ":") != null) {
            // Showing a file at a specific commit (e.g., HEAD:file.txt)
            return EmulatorResult.success("# Mock file content\n");
        }
    }

    // Default: show HEAD commit
    const output =
        \\commit a1b2c3d4e5f6789012345678901234567890abcd
        \\Author: EdgeBox Emulator <emulator@edgebox.dev>
        \\Date:   Wed Jan 1 00:00:00 2025 +0000
        \\
        \\    Initial commit
        \\
    ;

    return EmulatorResult.success(output);
}

fn emulateLsFiles(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = args;
    _ = mode;

    const output =
        \\README.md
        \\src/main.zig
        \\build.zig
        \\
    ;

    return EmulatorResult.success(output);
}

fn emulateVersion(mode: OutputMode) EmulatorResult {
    _ = mode;
    return EmulatorResult.success("git version 2.40.0 (EdgeBox Emulator)\n");
}

// ============================================================================
// Tests
// ============================================================================

test "git emulator - status" {
    const result = emulate(&.{"status"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@as(i32, 0), result.?.exit_code);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "nothing to commit") != null);
}

test "git emulator - status html mode" {
    const result = emulate(&.{"status"}, .browser, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "<div") != null);
}

test "git emulator - branch" {
    const result = emulate(&.{"branch"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "* main") != null);
}

test "git emulator - branch -a" {
    const result = emulate(&.{ "branch", "-a" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "remotes/origin") != null);
}

test "git emulator - log" {
    const result = emulate(&.{"log"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "commit") != null);
}

test "git emulator - log --oneline" {
    const result = emulate(&.{ "log", "--oneline" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "a1b2c3d") != null);
}

test "git emulator - remote -v" {
    const result = emulate(&.{ "remote", "-v" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "origin") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "github.com") != null);
}

test "git emulator - rev-parse HEAD" {
    const result = emulate(&.{ "rev-parse", "HEAD" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, mock_commit_hash) != null);
}

test "git emulator - rev-parse --abbrev-ref HEAD" {
    const result = emulate(&.{ "rev-parse", "--abbrev-ref", "HEAD" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("main\n", result.?.stdout);
}

test "git emulator - diff returns empty" {
    const result = emulate(&.{"diff"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("", result.?.stdout);
}

test "git emulator - config user.name" {
    const result = emulate(&.{ "config", "user.name" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("EdgeBox Emulator\n", result.?.stdout);
}

test "git emulator - --version" {
    const result = emulate(&.{"--version"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "EdgeBox Emulator") != null);
}

test "git emulator - unknown returns null" {
    const result = emulate(&.{"unknown-command"}, .server, std.testing.allocator);
    try std.testing.expect(result == null);
}

test "git emulator - empty args returns error" {
    const result = emulate(&.{}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.exit_code != 0);
}
