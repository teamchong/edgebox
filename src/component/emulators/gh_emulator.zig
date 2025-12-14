// GitHub CLI (gh) Command Emulator
//
// Provides mock implementations of common gh commands for sandboxed execution.
// Returns deterministic output without accessing real GitHub API.
//
// Supported subcommands:
// - pr list: List pull requests
// - pr view: View a pull request
// - issue list: List issues
// - issue view: View an issue
// - repo view: View repository info
// - auth status: Show auth status (mock - never reveals real tokens)

const std = @import("std");
const EmulatorResult = @import("mod.zig").EmulatorResult;
const OutputMode = @import("mod.zig").OutputMode;

// ============================================================================
// Mock Data
// ============================================================================

const mock_repo = "example/repo";
const mock_user = "edgebox-user";

// ============================================================================
// Main Entry Point
// ============================================================================

pub fn emulate(args: []const []const u8, mode: OutputMode, allocator: std.mem.Allocator) ?EmulatorResult {
    _ = allocator;

    if (args.len == 0) {
        return EmulatorResult.failure("usage: gh <command> [<args>]\n", 1);
    }

    const command = args[0];

    if (std.mem.eql(u8, command, "pr")) {
        return emulatePr(args[1..], mode);
    } else if (std.mem.eql(u8, command, "issue")) {
        return emulateIssue(args[1..], mode);
    } else if (std.mem.eql(u8, command, "repo")) {
        return emulateRepo(args[1..], mode);
    } else if (std.mem.eql(u8, command, "auth")) {
        return emulateAuth(args[1..], mode);
    } else if (std.mem.eql(u8, command, "--version")) {
        return emulateVersion(mode);
    } else if (std.mem.eql(u8, command, "api")) {
        return emulateApi(args[1..], mode);
    }

    // Unknown command - return null to fall through
    return null;
}

// ============================================================================
// PR Subcommands
// ============================================================================

fn emulatePr(args: []const []const u8, mode: OutputMode) ?EmulatorResult {
    if (args.len == 0) {
        return EmulatorResult.failure("usage: gh pr <command>\n", 1);
    }

    const subcommand = args[0];

    if (std.mem.eql(u8, subcommand, "list")) {
        return emulatePrList(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "view")) {
        return emulatePrView(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "status")) {
        return emulatePrStatus(mode);
    }

    return null;
}

fn emulatePrList(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = args;

    const text_output =
        \\#123  OPEN   Add new feature       feature-branch   2025-01-01
        \\#122  MERGED Fix critical bug      bugfix-branch    2025-01-01
        \\#121  CLOSED Update dependencies   deps-update      2024-12-31
        \\
    ;

    const html_output =
        \\<table class="pr-list">
        \\  <tr><td>#123</td><td class="open">OPEN</td><td>Add new feature</td></tr>
        \\  <tr><td>#122</td><td class="merged">MERGED</td><td>Fix critical bug</td></tr>
        \\  <tr><td>#121</td><td class="closed">CLOSED</td><td>Update dependencies</td></tr>
        \\</table>
    ;

    const viewkit_output =
        \\{"type":"pr-list","items":[
        \\  {"number":123,"state":"open","title":"Add new feature"},
        \\  {"number":122,"state":"merged","title":"Fix critical bug"},
        \\  {"number":121,"state":"closed","title":"Update dependencies"}
        \\]}
    ;

    return EmulatorResult.success(switch (mode) {
        .server => text_output,
        .browser => html_output,
        .viewkit => viewkit_output,
    });
}

fn emulatePrView(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = mode;
    _ = args;

    // TODO: Use pr_number from args when implementing dynamic PR responses

    const output =
        \\title:  Add new feature
        \\state:  OPEN
        \\author: edgebox-user
        \\branch: feature-branch -> main
        \\
        \\This PR adds a new feature to the application.
        \\
        \\Reviewers: none
        \\Assignees: none
        \\Labels: enhancement
        \\
    ;

    return EmulatorResult.success(output);
}

fn emulatePrStatus(mode: OutputMode) EmulatorResult {
    _ = mode;

    const output =
        \\Relevant pull requests in example/repo
        \\
        \\Current branch
        \\  #123 Add new feature [feature-branch]
        \\   - Checks passing
        \\
        \\Created by you
        \\  #123 Add new feature
        \\
        \\Requesting a code review from you
        \\  You have no pull requests to review
        \\
    ;

    return EmulatorResult.success(output);
}

// ============================================================================
// Issue Subcommands
// ============================================================================

fn emulateIssue(args: []const []const u8, mode: OutputMode) ?EmulatorResult {
    if (args.len == 0) {
        return EmulatorResult.failure("usage: gh issue <command>\n", 1);
    }

    const subcommand = args[0];

    if (std.mem.eql(u8, subcommand, "list")) {
        return emulateIssueList(args[1..], mode);
    } else if (std.mem.eql(u8, subcommand, "view")) {
        return emulateIssueView(args[1..], mode);
    }

    return null;
}

fn emulateIssueList(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = args;

    const text_output =
        \\#45   OPEN   Bug: Application crashes on startup   bug        2025-01-01
        \\#44   OPEN   Feature request: Dark mode            enhancement 2025-01-01
        \\#43   CLOSED Documentation update needed           docs        2024-12-31
        \\
    ;

    const html_output =
        \\<table class="issue-list">
        \\  <tr><td>#45</td><td class="open">OPEN</td><td>Bug: Application crashes on startup</td></tr>
        \\  <tr><td>#44</td><td class="open">OPEN</td><td>Feature request: Dark mode</td></tr>
        \\  <tr><td>#43</td><td class="closed">CLOSED</td><td>Documentation update needed</td></tr>
        \\</table>
    ;

    const viewkit_output =
        \\{"type":"issue-list","items":[
        \\  {"number":45,"state":"open","title":"Bug: Application crashes on startup"},
        \\  {"number":44,"state":"open","title":"Feature request: Dark mode"},
        \\  {"number":43,"state":"closed","title":"Documentation update needed"}
        \\]}
    ;

    return EmulatorResult.success(switch (mode) {
        .server => text_output,
        .browser => html_output,
        .viewkit => viewkit_output,
    });
}

fn emulateIssueView(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = args;
    _ = mode;

    const output =
        \\title:  Bug: Application crashes on startup
        \\state:  OPEN
        \\author: edgebox-user
        \\
        \\The application crashes immediately after starting.
        \\
        \\Steps to reproduce:
        \\1. Start the application
        \\2. Observe crash
        \\
        \\Labels: bug
        \\Assignees: none
        \\
    ;

    return EmulatorResult.success(output);
}

// ============================================================================
// Repo Subcommands
// ============================================================================

fn emulateRepo(args: []const []const u8, mode: OutputMode) ?EmulatorResult {
    if (args.len == 0) {
        return EmulatorResult.failure("usage: gh repo <command>\n", 1);
    }

    const subcommand = args[0];

    if (std.mem.eql(u8, subcommand, "view")) {
        return emulateRepoView(args[1..], mode);
    }

    return null;
}

fn emulateRepoView(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = args;

    const text_output =
        \\example/repo
        \\A sample repository for demonstration
        \\
        \\  Stars:      42
        \\  Forks:      10
        \\  Open issues: 5
        \\  Watchers:   15
        \\
        \\View this repository on GitHub: https://github.com/example/repo
        \\
    ;

    const html_output =
        \\<div class="repo-view">
        \\  <h2>example/repo</h2>
        \\  <p>A sample repository for demonstration</p>
        \\  <ul>
        \\    <li>Stars: 42</li>
        \\    <li>Forks: 10</li>
        \\    <li>Open issues: 5</li>
        \\  </ul>
        \\</div>
    ;

    const viewkit_output =
        \\{"type":"repo","name":"example/repo","description":"A sample repository for demonstration","stars":42,"forks":10,"issues":5}
    ;

    return EmulatorResult.success(switch (mode) {
        .server => text_output,
        .browser => html_output,
        .viewkit => viewkit_output,
    });
}

// ============================================================================
// Auth Subcommands
// ============================================================================

fn emulateAuth(args: []const []const u8, mode: OutputMode) ?EmulatorResult {
    if (args.len == 0) {
        return EmulatorResult.failure("usage: gh auth <command>\n", 1);
    }

    const subcommand = args[0];

    if (std.mem.eql(u8, subcommand, "status")) {
        return emulateAuthStatus(mode);
    } else if (std.mem.eql(u8, subcommand, "login")) {
        // Block login - emulator doesn't support real auth
        return EmulatorResult.failure("Error: gh auth login is not supported in emulator mode\n", 1);
    } else if (std.mem.eql(u8, subcommand, "logout")) {
        // Block logout
        return EmulatorResult.failure("Error: gh auth logout is not supported in emulator mode\n", 1);
    }

    return null;
}

fn emulateAuthStatus(mode: OutputMode) EmulatorResult {
    _ = mode;

    const output =
        \\github.com
        \\  ✓ Logged in to github.com as edgebox-user (emulator mode)
        \\  ✓ Git operations for github.com configured to use https protocol.
        \\  ✓ Token: **************** (emulated)
        \\
    ;

    return EmulatorResult.success(output);
}

// ============================================================================
// API Subcommand
// ============================================================================

fn emulateApi(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = mode;

    // Return mock JSON for common API endpoints
    for (args) |arg| {
        if (std.mem.indexOf(u8, arg, "/user") != null) {
            return EmulatorResult.success(
                \\{"login":"edgebox-user","id":12345,"type":"User"}
                \\
            );
        } else if (std.mem.indexOf(u8, arg, "/repos") != null) {
            return EmulatorResult.success(
                \\{"name":"repo","full_name":"example/repo","private":false}
                \\
            );
        }
    }

    // Generic response
    return EmulatorResult.success("{\"emulated\":true}\n");
}

// ============================================================================
// Version
// ============================================================================

fn emulateVersion(mode: OutputMode) EmulatorResult {
    _ = mode;
    return EmulatorResult.success("gh version 2.40.0 (EdgeBox Emulator)\n");
}

// ============================================================================
// Tests
// ============================================================================

test "gh emulator - pr list" {
    const result = emulate(&.{ "pr", "list" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@as(i32, 0), result.?.exit_code);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "#123") != null);
}

test "gh emulator - pr list html mode" {
    const result = emulate(&.{ "pr", "list" }, .browser, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "<table") != null);
}

test "gh emulator - pr view" {
    const result = emulate(&.{ "pr", "view", "123" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "Add new feature") != null);
}

test "gh emulator - issue list" {
    const result = emulate(&.{ "issue", "list" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "#45") != null);
}

test "gh emulator - repo view" {
    const result = emulate(&.{ "repo", "view" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "example/repo") != null);
}

test "gh emulator - auth status" {
    const result = emulate(&.{ "auth", "status" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "Logged in") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "emulator mode") != null);
}

test "gh emulator - auth login blocked" {
    const result = emulate(&.{ "auth", "login" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.exit_code != 0);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stderr, "not supported") != null);
}

test "gh emulator - --version" {
    const result = emulate(&.{"--version"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "EdgeBox Emulator") != null);
}

test "gh emulator - api endpoint" {
    const result = emulate(&.{ "api", "/user" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "edgebox-user") != null);
}

test "gh emulator - unknown returns null" {
    const result = emulate(&.{"unknown-command"}, .server, std.testing.allocator);
    try std.testing.expect(result == null);
}

test "gh emulator - empty args returns error" {
    const result = emulate(&.{}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.exit_code != 0);
}
