// Security helper function tests for Shell Emulation
// Run with: zig test src/security_test.zig
//
// These tests verify the security features work correctly:
// 1. Blocked pattern detection (isCommandDestructive)
// 2. Sensitive file access blocking (accessesSensitiveFile)
// 3. Binary name extraction (extractBinaryName)

const std = @import("std");

// ============================================================================
// Security Helper Functions (copied from edgebox_wamr.zig for standalone testing)
// ============================================================================

/// Check if command matches destructive patterns from config
/// Returns true if command should be blocked
fn isCommandDestructive(cmd: []const u8, blocked_patterns: []const []const u8) bool {
    // Check blocked patterns (simple substring matching for MVP)
    for (blocked_patterns) |pattern| {
        if (std.mem.indexOf(u8, cmd, pattern)) |_| {
            return true;
        }
    }
    return false;
}

/// Check if command tries to access sensitive files
/// Returns true if command should be blocked
fn accessesSensitiveFile(cmd: []const u8, sensitive_files: []const []const u8) bool {
    // Simple pattern matching for MVP
    for (sensitive_files) |pattern| {
        // Remove glob wildcards for simple matching
        var clean_pattern = pattern;
        if (std.mem.startsWith(u8, pattern, "**/")) {
            clean_pattern = pattern[3..];
        }
        if (std.mem.endsWith(u8, clean_pattern, "/*")) {
            clean_pattern = clean_pattern[0 .. clean_pattern.len - 2];
        }

        // Handle leading wildcard (*.pem -> .pem)
        if (clean_pattern.len > 0 and clean_pattern[0] == '*') {
            const suffix = clean_pattern[1..];
            if (std.mem.indexOf(u8, cmd, suffix)) |_| {
                return true;
            }
        } else {
            // Exact match or substring
            if (std.mem.indexOf(u8, cmd, clean_pattern)) |_| {
                return true;
            }
        }
    }
    return false;
}

/// Extract binary name from command string
/// Example: "gh pr list" -> "gh"
fn extractBinaryName(cmd: []const u8) []const u8 {
    // Find first space or end of string
    if (std.mem.indexOf(u8, cmd, " ")) |space_idx| {
        return cmd[0..space_idx];
    }
    return cmd;
}

// ============================================================================
// Tests: isCommandDestructive - Blocked Pattern Detection
// ============================================================================

test "isCommandDestructive - blocks git reset --hard" {
    const blocked = &[_][]const u8{
        "git reset --hard",
        "rm -rf",
        "> .env",
    };
    // Should block
    try std.testing.expect(isCommandDestructive("git reset --hard HEAD", blocked));
    try std.testing.expect(isCommandDestructive("git reset --hard", blocked));
    // Should allow
    try std.testing.expect(!isCommandDestructive("git reset --soft", blocked));
    try std.testing.expect(!isCommandDestructive("git reset --soft HEAD", blocked));
    try std.testing.expect(!isCommandDestructive("git status", blocked));
    try std.testing.expect(!isCommandDestructive("git log", blocked));
}

test "isCommandDestructive - blocks rm -rf" {
    const blocked = &[_][]const u8{
        "rm -rf",
        "git reset --hard",
    };
    // Should block
    try std.testing.expect(isCommandDestructive("rm -rf /", blocked));
    try std.testing.expect(isCommandDestructive("rm -rf .", blocked));
    try std.testing.expect(isCommandDestructive("rm -rf /etc", blocked));
    try std.testing.expect(isCommandDestructive("rm -rf ~/", blocked));
    // Should allow
    try std.testing.expect(!isCommandDestructive("rm file.txt", blocked));
    try std.testing.expect(!isCommandDestructive("rm -r dir", blocked));
    try std.testing.expect(!isCommandDestructive("rm -f file.txt", blocked));
    try std.testing.expect(!isCommandDestructive("rmdir empty_dir", blocked));
}

test "isCommandDestructive - blocks shell redirects to .env" {
    const blocked = &[_][]const u8{
        "> .env",
        ">> .env",
    };
    // Should block - these would overwrite/append to .env
    try std.testing.expect(isCommandDestructive("echo foo > .env", blocked));
    try std.testing.expect(isCommandDestructive("echo bar >> .env", blocked));
    try std.testing.expect(isCommandDestructive("cat secrets > .env", blocked));
    // Should allow - reading .env is ok, writing to other files is ok
    try std.testing.expect(!isCommandDestructive("cat .env", blocked));
    try std.testing.expect(!isCommandDestructive("echo foo > output.txt", blocked));
    try std.testing.expect(!isCommandDestructive("source .env", blocked));
}

test "isCommandDestructive - blocks sudo rm -rf (partial match)" {
    const blocked = &[_][]const u8{
        "rm -rf",
    };
    // Partial match should still block - pattern found anywhere in command
    try std.testing.expect(isCommandDestructive("sudo rm -rf /etc", blocked));
    try std.testing.expect(isCommandDestructive("bash -c 'rm -rf /'", blocked));
}

test "isCommandDestructive - blocks force push to main" {
    const blocked = &[_][]const u8{
        "git push --force origin main",
        "git push -f origin main",
        "git push --force-with-lease origin main",
    };
    try std.testing.expect(isCommandDestructive("git push --force origin main", blocked));
    try std.testing.expect(isCommandDestructive("git push -f origin main", blocked));
    // Should allow regular push
    try std.testing.expect(!isCommandDestructive("git push origin main", blocked));
    try std.testing.expect(!isCommandDestructive("git push origin feature-branch", blocked));
}

test "isCommandDestructive - empty blocked list allows all" {
    const blocked = &[_][]const u8{};
    try std.testing.expect(!isCommandDestructive("rm -rf /", blocked));
    try std.testing.expect(!isCommandDestructive("git reset --hard", blocked));
    try std.testing.expect(!isCommandDestructive("echo foo > .env", blocked));
}

// ============================================================================
// Tests: accessesSensitiveFile - Sensitive File Access Detection
// ============================================================================

test "accessesSensitiveFile - blocks .env" {
    const sensitive = &[_][]const u8{
        ".env",
        "*.pem",
        "credentials.json",
    };
    // Should block
    try std.testing.expect(accessesSensitiveFile("cat .env", sensitive));
    try std.testing.expect(accessesSensitiveFile("less .env", sensitive));
    try std.testing.expect(accessesSensitiveFile("vim .env", sensitive));
    try std.testing.expect(accessesSensitiveFile("head -10 .env", sensitive));
    // Should allow
    try std.testing.expect(!accessesSensitiveFile("cat README.md", sensitive));
    try std.testing.expect(!accessesSensitiveFile("ls -la", sensitive));
}

test "accessesSensitiveFile - blocks wildcard patterns (*.pem)" {
    const sensitive = &[_][]const u8{
        "*.pem",
        "*.key",
    };
    // Should block
    try std.testing.expect(accessesSensitiveFile("cat server.pem", sensitive));
    try std.testing.expect(accessesSensitiveFile("cat private.key", sensitive));
    try std.testing.expect(accessesSensitiveFile("cat /etc/ssl/certs/ca.pem", sensitive));
    try std.testing.expect(accessesSensitiveFile("openssl x509 -in cert.pem", sensitive));
    // Should allow
    try std.testing.expect(!accessesSensitiveFile("cat config.json", sensitive));
    try std.testing.expect(!accessesSensitiveFile("cat package.json", sensitive));
}

test "accessesSensitiveFile - blocks credentials.json" {
    const sensitive = &[_][]const u8{
        "credentials.json",
        ".env",
    };
    // Should block
    try std.testing.expect(accessesSensitiveFile("cat credentials.json", sensitive));
    try std.testing.expect(accessesSensitiveFile("jq . credentials.json", sensitive));
    // Should allow
    try std.testing.expect(!accessesSensitiveFile("cat package.json", sensitive));
    try std.testing.expect(!accessesSensitiveFile("cat tsconfig.json", sensitive));
}

test "accessesSensitiveFile - blocks SSH keys" {
    const sensitive = &[_][]const u8{
        ".ssh",
        "id_rsa",
        "id_ed25519",
    };
    // Should block
    try std.testing.expect(accessesSensitiveFile("cat ~/.ssh/id_rsa", sensitive));
    try std.testing.expect(accessesSensitiveFile("cat ~/.ssh/id_ed25519", sensitive));
    try std.testing.expect(accessesSensitiveFile("cat /home/user/.ssh/config", sensitive));
    // Should allow
    try std.testing.expect(!accessesSensitiveFile("ssh-keygen -t ed25519", sensitive));
    try std.testing.expect(!accessesSensitiveFile("cat ~/.bashrc", sensitive));
}

test "accessesSensitiveFile - blocks AWS credentials" {
    const sensitive = &[_][]const u8{
        ".aws/credentials",
        "aws_access_key",
        "aws_secret_key",
    };
    // Should block
    try std.testing.expect(accessesSensitiveFile("cat ~/.aws/credentials", sensitive));
    try std.testing.expect(accessesSensitiveFile("grep aws_access_key config", sensitive));
    // Should allow
    try std.testing.expect(!accessesSensitiveFile("aws s3 ls", sensitive));
    try std.testing.expect(!accessesSensitiveFile("cat ~/.aws/config", sensitive));
}

test "accessesSensitiveFile - empty sensitive list allows all" {
    const sensitive = &[_][]const u8{};
    try std.testing.expect(!accessesSensitiveFile("cat .env", sensitive));
    try std.testing.expect(!accessesSensitiveFile("cat ~/.ssh/id_rsa", sensitive));
    try std.testing.expect(!accessesSensitiveFile("cat credentials.json", sensitive));
}

// ============================================================================
// Tests: extractBinaryName - Binary Name Extraction
// ============================================================================

test "extractBinaryName - extracts first word" {
    try std.testing.expectEqualStrings("git", extractBinaryName("git status"));
    try std.testing.expectEqualStrings("echo", extractBinaryName("echo hello world"));
    try std.testing.expectEqualStrings("ls", extractBinaryName("ls -la /tmp"));
    try std.testing.expectEqualStrings("grep", extractBinaryName("grep -r pattern ."));
}

test "extractBinaryName - single word command" {
    try std.testing.expectEqualStrings("cat", extractBinaryName("cat"));
    try std.testing.expectEqualStrings("ls", extractBinaryName("ls"));
    try std.testing.expectEqualStrings("pwd", extractBinaryName("pwd"));
}

test "extractBinaryName - handles absolute paths" {
    try std.testing.expectEqualStrings("/usr/bin/git", extractBinaryName("/usr/bin/git status"));
    try std.testing.expectEqualStrings("/bin/bash", extractBinaryName("/bin/bash -c 'echo hi'"));
    try std.testing.expectEqualStrings("/opt/homebrew/bin/node", extractBinaryName("/opt/homebrew/bin/node script.js"));
}

test "extractBinaryName - handles relative paths" {
    try std.testing.expectEqualStrings("./script.sh", extractBinaryName("./script.sh arg1 arg2"));
    try std.testing.expectEqualStrings("../bin/app", extractBinaryName("../bin/app --help"));
}

// ============================================================================
// Tests: Edge Cases and Boundary Conditions
// ============================================================================

test "edge case - command with multiple consecutive spaces" {
    const blocked = &[_][]const u8{"rm -rf"};
    // The pattern should still match even with extra spaces in other parts
    try std.testing.expect(isCommandDestructive("rm -rf  /tmp", blocked));
}

test "edge case - case sensitivity" {
    const blocked = &[_][]const u8{"rm -rf"};
    // Should be case-sensitive (common shell behavior)
    try std.testing.expect(isCommandDestructive("rm -rf /", blocked));
    try std.testing.expect(!isCommandDestructive("RM -RF /", blocked));
    try std.testing.expect(!isCommandDestructive("Rm -Rf /", blocked));
}

test "edge case - pattern at different positions" {
    const blocked = &[_][]const u8{"dangerous"};
    // Pattern can appear anywhere in command
    try std.testing.expect(isCommandDestructive("dangerous command", blocked));
    try std.testing.expect(isCommandDestructive("run dangerous", blocked));
    try std.testing.expect(isCommandDestructive("run --dangerous flag", blocked));
    try std.testing.expect(isCommandDestructive("cmd dangerous_file.txt", blocked));
}

test "edge case - empty command" {
    const blocked = &[_][]const u8{"rm -rf"};
    const sensitive = &[_][]const u8{".env"};
    try std.testing.expect(!isCommandDestructive("", blocked));
    try std.testing.expect(!accessesSensitiveFile("", sensitive));
    try std.testing.expectEqualStrings("", extractBinaryName(""));
}

test "edge case - very long command" {
    const blocked = &[_][]const u8{"rm -rf"};
    const long_cmd = "echo " ++ "a" ** 1000 ++ " && rm -rf /";
    try std.testing.expect(isCommandDestructive(long_cmd, blocked));
}

test "edge case - unicode in command" {
    const blocked = &[_][]const u8{"rm -rf"};
    // Unicode characters shouldn't affect pattern matching
    try std.testing.expect(!isCommandDestructive("echo 你好", blocked));
    try std.testing.expect(isCommandDestructive("rm -rf /tmp/文件", blocked));
}

test "edge case - newlines and tabs in command" {
    const blocked = &[_][]const u8{"rm -rf"};
    // Commands with embedded newlines/tabs
    try std.testing.expect(isCommandDestructive("echo hi\nrm -rf /", blocked));
    try std.testing.expect(isCommandDestructive("echo hi\trm -rf /", blocked));
}

// ============================================================================
// Tests: Real-world Attack Patterns
// ============================================================================

test "attack pattern - command injection via semicolon" {
    const blocked = &[_][]const u8{"rm -rf", "; rm"};
    try std.testing.expect(isCommandDestructive("echo test; rm -rf /", blocked));
    try std.testing.expect(isCommandDestructive("ls; rm -rf /tmp", blocked));
}

test "attack pattern - command injection via backticks" {
    const blocked = &[_][]const u8{"`rm", "$(rm"};
    try std.testing.expect(isCommandDestructive("echo `rm -rf /`", blocked));
    try std.testing.expect(isCommandDestructive("echo $(rm -rf /)", blocked));
}

test "attack pattern - pipe to destructive command" {
    const blocked = &[_][]const u8{"| rm", "| xargs rm"};
    try std.testing.expect(isCommandDestructive("find . | rm", blocked));
    try std.testing.expect(isCommandDestructive("find . -name '*.tmp' | xargs rm", blocked));
}

test "attack pattern - environment variable exfiltration" {
    const sensitive = &[_][]const u8{
        "$AWS_SECRET",
        "$ANTHROPIC_API_KEY",
        "$GH_TOKEN",
    };
    try std.testing.expect(accessesSensitiveFile("echo $AWS_SECRET", sensitive));
    try std.testing.expect(accessesSensitiveFile("curl -H \"Authorization: $ANTHROPIC_API_KEY\"", sensitive));
    try std.testing.expect(accessesSensitiveFile("gh auth login --with-token <<< $GH_TOKEN", sensitive));
}
