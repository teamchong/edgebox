// NPM Command Emulator
//
// Provides mock implementations of common npm commands for sandboxed execution.
// Returns deterministic output without accessing real npm registry or package.json.
//
// Supported subcommands:
// - --version: Version string
// - list: Mock package tree
// - run <script>: Success message
// - test: Mock test output
// - install: Mock install output
// - info: Mock package info

const std = @import("std");
const EmulatorResult = @import("mod.zig").EmulatorResult;
const OutputMode = @import("mod.zig").OutputMode;

// ============================================================================
// Mock Data
// ============================================================================

const mock_npm_version = "10.2.0";
const mock_node_version = "20.10.0";
const mock_package_name = "example-app";
const mock_package_version = "1.0.0";

// ============================================================================
// Main Entry Point
// ============================================================================

pub fn emulate(args: []const []const u8, mode: OutputMode, allocator: std.mem.Allocator) ?EmulatorResult {
    _ = allocator;

    if (args.len == 0) {
        return EmulatorResult.failure("Usage: npm <command>\n", 1);
    }

    const command = args[0];

    if (std.mem.eql(u8, command, "--version") or std.mem.eql(u8, command, "-v")) {
        return emulateVersion(mode);
    } else if (std.mem.eql(u8, command, "list") or std.mem.eql(u8, command, "ls")) {
        return emulateList(args[1..], mode);
    } else if (std.mem.eql(u8, command, "run")) {
        return emulateRun(args[1..], mode);
    } else if (std.mem.eql(u8, command, "test") or std.mem.eql(u8, command, "t")) {
        return emulateTest(mode);
    } else if (std.mem.eql(u8, command, "install") or std.mem.eql(u8, command, "i")) {
        return emulateInstall(args[1..], mode);
    } else if (std.mem.eql(u8, command, "info") or std.mem.eql(u8, command, "view")) {
        return emulateInfo(args[1..], mode);
    } else if (std.mem.eql(u8, command, "init")) {
        return emulateInit(mode);
    } else if (std.mem.eql(u8, command, "outdated")) {
        return emulateOutdated(mode);
    } else if (std.mem.eql(u8, command, "audit")) {
        return emulateAudit(mode);
    } else if (std.mem.eql(u8, command, "config")) {
        if (emulateConfig(args[1..], mode)) |result| {
            return result;
        }
        return null;
    } else if (std.mem.eql(u8, command, "whoami")) {
        return emulateWhoami(mode);
    } else if (std.mem.eql(u8, command, "publish")) {
        // Block publish - emulator doesn't support real publishing
        return EmulatorResult.failure("Error: npm publish is not supported in emulator mode\n", 1);
    } else if (std.mem.eql(u8, command, "login") or std.mem.eql(u8, command, "adduser")) {
        // Block login - emulator doesn't support real auth
        return EmulatorResult.failure("Error: npm login is not supported in emulator mode\n", 1);
    }

    // Unknown command - return null to fall through
    return null;
}

// ============================================================================
// Subcommand Implementations
// ============================================================================

fn emulateVersion(mode: OutputMode) EmulatorResult {
    const text_output = mock_npm_version ++ "\n";

    const html_output =
        \\<div class="npm-version">
        \\  <span class="version">10.2.0</span>
        \\</div>
    ;

    const viewkit_output =
        \\{"type":"version","npm":"10.2.0","node":"20.10.0"}
    ;

    return EmulatorResult.success(switch (mode) {
        .server => text_output,
        .browser => html_output,
        .viewkit => viewkit_output,
    });
}

fn emulateList(args: []const []const u8, mode: OutputMode) EmulatorResult {
    // Check for --depth flag (reserved for future use)
    _ = args;

    const text_output =
        \\example-app@1.0.0 /mock/project
        \\+-- express@4.18.2
        \\+-- lodash@4.17.21
        \\`-- typescript@5.3.0
        \\
    ;

    const html_output =
        \\<div class="npm-list">
        \\  <h3>example-app@1.0.0</h3>
        \\  <ul>
        \\    <li>express@4.18.2</li>
        \\    <li>lodash@4.17.21</li>
        \\    <li>typescript@5.3.0</li>
        \\  </ul>
        \\</div>
    ;

    const viewkit_output =
        \\{"type":"package-list","name":"example-app","version":"1.0.0","dependencies":[
        \\  {"name":"express","version":"4.18.2"},
        \\  {"name":"lodash","version":"4.17.21"},
        \\  {"name":"typescript","version":"5.3.0"}
        \\]}
    ;

    return EmulatorResult.success(switch (mode) {
        .server => text_output,
        .browser => html_output,
        .viewkit => viewkit_output,
    });
}

fn emulateRun(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = mode;

    if (args.len == 0) {
        // List available scripts
        const output =
            \\Lifecycle scripts included in example-app@1.0.0:
            \\  test
            \\    jest
            \\  start
            \\    node server.js
            \\
            \\available via `npm run-script`:
            \\  build
            \\    tsc
            \\  lint
            \\    eslint .
            \\
        ;
        return EmulatorResult.success(output);
    }

    const script_name = args[0];

    if (std.mem.eql(u8, script_name, "build")) {
        return EmulatorResult.success(
            \\> example-app@1.0.0 build
            \\> tsc
            \\
            \\Build completed successfully.
            \\
        );
    } else if (std.mem.eql(u8, script_name, "start")) {
        return EmulatorResult.success(
            \\> example-app@1.0.0 start
            \\> node server.js
            \\
            \\Server started on port 3000 (emulated)
            \\
        );
    } else if (std.mem.eql(u8, script_name, "lint")) {
        return EmulatorResult.success(
            \\> example-app@1.0.0 lint
            \\> eslint .
            \\
            \\No linting errors found.
            \\
        );
    }

    // Generic success for unknown scripts
    return EmulatorResult.success(
        \\> example-app@1.0.0 script
        \\Script completed successfully (emulated)
        \\
    );
}

fn emulateTest(mode: OutputMode) EmulatorResult {
    const text_output =
        \\> example-app@1.0.0 test
        \\> jest
        \\
        \\ PASS  src/utils.test.js
        \\ PASS  src/api.test.js
        \\
        \\Test Suites: 2 passed, 2 total
        \\Tests:       8 passed, 8 total
        \\Snapshots:   0 total
        \\Time:        1.234 s
        \\
    ;

    const html_output =
        \\<div class="npm-test">
        \\  <h3>Test Results</h3>
        \\  <p class="pass">PASS src/utils.test.js</p>
        \\  <p class="pass">PASS src/api.test.js</p>
        \\  <p class="summary">Tests: 8 passed, 8 total</p>
        \\</div>
    ;

    const viewkit_output =
        \\{"type":"test-results","passed":8,"failed":0,"suites":2,"time":"1.234s"}
    ;

    return EmulatorResult.success(switch (mode) {
        .server => text_output,
        .browser => html_output,
        .viewkit => viewkit_output,
    });
}

fn emulateInstall(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = mode;

    if (args.len == 0) {
        // npm install (no package specified)
        return EmulatorResult.success(
            \\added 150 packages in 3.5s
            \\
            \\50 packages are looking for funding
            \\  run `npm fund` for details
            \\
        );
    }

    // npm install <package>
    const package_name = args[0];
    _ = package_name;

    return EmulatorResult.success(
        \\added 1 package in 0.5s
        \\
    );
}

fn emulateInfo(args: []const []const u8, mode: OutputMode) EmulatorResult {
    _ = mode;

    if (args.len == 0) {
        return EmulatorResult.failure("Usage: npm info <package>\n", 1);
    }

    const package_name = args[0];
    _ = package_name;

    const output =
        \\example-package@1.0.0 | MIT | deps: 2 | versions: 10
        \\An example package for demonstration
        \\https://github.com/example/package
        \\
        \\dist-tags:
        \\latest: 1.0.0
        \\
        \\maintainers:
        \\- example-user <user@example.com>
        \\
    ;

    return EmulatorResult.success(output);
}

fn emulateInit(mode: OutputMode) EmulatorResult {
    _ = mode;

    // Block init - it's interactive
    return EmulatorResult.failure("Error: npm init is not supported in emulator mode (requires interactive input)\n", 1);
}

fn emulateOutdated(mode: OutputMode) EmulatorResult {
    const text_output =
        \\Package      Current  Wanted  Latest  Location             Depended by
        \\lodash       4.17.20  4.17.21 4.17.21 node_modules/lodash  example-app
        \\typescript   5.2.0    5.3.0   5.3.0   node_modules/typescript example-app
        \\
    ;

    const html_output =
        \\<table class="npm-outdated">
        \\  <tr><th>Package</th><th>Current</th><th>Wanted</th><th>Latest</th></tr>
        \\  <tr><td>lodash</td><td>4.17.20</td><td>4.17.21</td><td>4.17.21</td></tr>
        \\  <tr><td>typescript</td><td>5.2.0</td><td>5.3.0</td><td>5.3.0</td></tr>
        \\</table>
    ;

    const viewkit_output =
        \\{"type":"outdated","packages":[
        \\  {"name":"lodash","current":"4.17.20","wanted":"4.17.21","latest":"4.17.21"},
        \\  {"name":"typescript","current":"5.2.0","wanted":"5.3.0","latest":"5.3.0"}
        \\]}
    ;

    return EmulatorResult.success(switch (mode) {
        .server => text_output,
        .browser => html_output,
        .viewkit => viewkit_output,
    });
}

fn emulateAudit(mode: OutputMode) EmulatorResult {
    const text_output =
        \\found 0 vulnerabilities
        \\
    ;

    const html_output =
        \\<div class="npm-audit">
        \\  <p class="success">No vulnerabilities found</p>
        \\</div>
    ;

    const viewkit_output =
        \\{"type":"audit","vulnerabilities":0,"info":0,"low":0,"moderate":0,"high":0,"critical":0}
    ;

    return EmulatorResult.success(switch (mode) {
        .server => text_output,
        .browser => html_output,
        .viewkit => viewkit_output,
    });
}

fn emulateConfig(args: []const []const u8, mode: OutputMode) ?EmulatorResult {
    _ = mode;

    if (args.len == 0) {
        return EmulatorResult.failure("Usage: npm config <command>\n", 1);
    }

    const subcommand = args[0];

    if (std.mem.eql(u8, subcommand, "list")) {
        return EmulatorResult.success(
            \\; "user" config from ~/.npmrc
            \\
            \\registry = "https://registry.npmjs.org/"
            \\
            \\; node bin location = /usr/local/bin/node
            \\; node version = v20.10.0
            \\; npm local prefix = /mock/project
            \\; npm version = 10.2.0
            \\
        );
    } else if (std.mem.eql(u8, subcommand, "get") and args.len > 1) {
        const key = args[1];
        if (std.mem.eql(u8, key, "registry")) {
            return EmulatorResult.success("https://registry.npmjs.org/\n");
        }
        return EmulatorResult.success("undefined\n");
    } else if (std.mem.eql(u8, subcommand, "set")) {
        // Block config set - emulator doesn't persist config
        return EmulatorResult.failure("Error: npm config set is not supported in emulator mode\n", 1);
    }

    return null;
}

fn emulateWhoami(mode: OutputMode) EmulatorResult {
    _ = mode;
    return EmulatorResult.success("edgebox-user (emulated)\n");
}

// ============================================================================
// Tests
// ============================================================================

test "npm emulator - --version" {
    const result = emulate(&.{"--version"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expectEqual(@as(i32, 0), result.?.exit_code);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "10.2.0") != null);
}

test "npm emulator - list" {
    const result = emulate(&.{"list"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "example-app") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "express") != null);
}

test "npm emulator - list html mode" {
    const result = emulate(&.{"list"}, .browser, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "<div") != null);
}

test "npm emulator - run without script lists scripts" {
    const result = emulate(&.{"run"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "test") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "build") != null);
}

test "npm emulator - run build" {
    const result = emulate(&.{ "run", "build" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "Build completed") != null);
}

test "npm emulator - test" {
    const result = emulate(&.{"test"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "PASS") != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "8 passed") != null);
}

test "npm emulator - install" {
    const result = emulate(&.{"install"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "added") != null);
}

test "npm emulator - outdated" {
    const result = emulate(&.{"outdated"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "lodash") != null);
}

test "npm emulator - audit" {
    const result = emulate(&.{"audit"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "0 vulnerabilities") != null);
}

test "npm emulator - config list" {
    const result = emulate(&.{ "config", "list" }, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "registry") != null);
}

test "npm emulator - whoami" {
    const result = emulate(&.{"whoami"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stdout, "edgebox-user") != null);
}

test "npm emulator - publish blocked" {
    const result = emulate(&.{"publish"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.exit_code != 0);
    try std.testing.expect(std.mem.indexOf(u8, result.?.stderr, "not supported") != null);
}

test "npm emulator - login blocked" {
    const result = emulate(&.{"login"}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.exit_code != 0);
}

test "npm emulator - unknown returns null" {
    const result = emulate(&.{"unknown-command"}, .server, std.testing.allocator);
    try std.testing.expect(result == null);
}

test "npm emulator - empty args returns error" {
    const result = emulate(&.{}, .server, std.testing.allocator);
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.exit_code != 0);
}
