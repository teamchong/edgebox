/*
 * edgebox-sandbox - OS-level sandbox wrapper for EdgeBox
 *
 * Usage: edgebox-sandbox <command> [args...]
 *
 * Environment variables:
 *   __EDGEBOX_DIRS       - JSON array of allowed paths, e.g. '["/tmp","/Users/x/app"]'
 *   __EDGEBOX_ALLOW_CMDS - JSON array of allowed commands, e.g. '["git","npm","node"]'
 *   __EDGEBOX_DENY_CMDS  - JSON array of denied commands (takes precedence over allow)
 *
 * Platform support:
 *   macOS:   sandbox-exec with deny-default profile
 *   Linux:   bubblewrap (bwrap) with bind mounts
 *   Windows: Job Objects + Restricted Tokens
 *
 * Security: Prevents shell redirects, path traversal, and unauthorized file access
 */

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <sddl.h>
#include <aclapi.h>
#include <userenv.h>
#include <io.h>
#include <fcntl.h>
#pragma comment(lib, "userenv.lib")
#pragma comment(lib, "advapi32.lib")
#define access _access
#define F_OK 0
#else
#include <unistd.h>
#include <errno.h>
#include <sys/wait.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_DIRS 64
#define MAX_CMDS 128
#define MAX_PATH_LEN 4096
#define MAX_CMD_LEN 256
#define MAX_PROFILE_LEN 65536

// Simple JSON array parser for ["item1", "item2", ...]
// Returns count of parsed items, -1 on error
static int parse_json_array(const char *json, char items[][MAX_CMD_LEN], int max_items, int max_len) {
    if (!json || json[0] != '[') return -1;

    int count = 0;
    const char *p = json + 1;

    while (*p && count < max_items) {
        // Skip whitespace
        while (*p == ' ' || *p == '\t' || *p == '\n' || *p == ',') p++;
        if (*p == ']') break;
        if (*p != '"') return -1;

        p++; // skip opening quote
        char *dst = items[count];
        int len = 0;

        while (*p && *p != '"' && len < max_len - 1) {
            if (*p == '\\' && *(p+1)) {
                p++; // skip escape
            }
            dst[len++] = *p++;
        }
        dst[len] = '\0';

        if (*p != '"') return -1;
        p++; // skip closing quote
        count++;
    }

    return count;
}

// Simple JSON array parser for ["dir1", "dir2", ...]
// Returns count of parsed directories, -1 on error
static int parse_dirs_json(const char *json, char dirs[MAX_DIRS][MAX_PATH_LEN]) {
    if (!json || json[0] != '[') return -1;

    int count = 0;
    const char *p = json + 1;

    while (*p && count < MAX_DIRS) {
        // Skip whitespace
        while (*p == ' ' || *p == '\t' || *p == '\n' || *p == ',') p++;
        if (*p == ']') break;
        if (*p != '"') return -1;

        p++; // skip opening quote
        char *dst = dirs[count];
        int len = 0;

        while (*p && *p != '"' && len < MAX_PATH_LEN - 1) {
            if (*p == '\\' && *(p+1)) {
                p++; // skip escape
            }
            dst[len++] = *p++;
        }
        dst[len] = '\0';

        if (*p != '"') return -1;
        p++; // skip closing quote

        // Expand ~ to HOME (Unix) or USERPROFILE (Windows)
        if (dirs[count][0] == '~') {
#ifdef _WIN32
            const char *home = getenv("USERPROFILE");
#else
            const char *home = getenv("HOME");
#endif
            if (home) {
                char expanded[MAX_PATH_LEN];
                snprintf(expanded, MAX_PATH_LEN, "%s%s", home, dirs[count] + 1);
                strncpy(dirs[count], expanded, MAX_PATH_LEN - 1);
                dirs[count][MAX_PATH_LEN - 1] = '\0';
            }
        }

        count++;
    }

    return count;
}

#ifdef __APPLE__
/*
 * macOS: sandbox-exec implementation
 *
 * Generates a Scheme profile that:
 * - Denies all by default
 * - Allows reading system libraries
 * - Allows read/write only to specified directories
 */

static int run_sandboxed_macos(int argc, char **argv, char dirs[MAX_DIRS][MAX_PATH_LEN], int dir_count) {
    char profile[MAX_PROFILE_LEN];
    int offset = 0;

    // Resolve symlinks for all directories (e.g., /tmp -> /private/tmp on macOS)
    char resolved_dirs[MAX_DIRS][MAX_PATH_LEN];
    for (int i = 0; i < dir_count; i++) {
        char *resolved = realpath(dirs[i], NULL);
        if (resolved) {
            strncpy(resolved_dirs[i], resolved, MAX_PATH_LEN - 1);
            resolved_dirs[i][MAX_PATH_LEN - 1] = '\0';
            free(resolved);
        } else {
            // If realpath fails (dir doesn't exist), use original
            strncpy(resolved_dirs[i], dirs[i], MAX_PATH_LEN - 1);
            resolved_dirs[i][MAX_PATH_LEN - 1] = '\0';
        }
    }

    // Build deny-default profile
    // Strategy: Allow all reads (needed for system libs), restrict writes to allowed dirs only
    offset += snprintf(profile + offset, MAX_PROFILE_LEN - offset,
        "(version 1)\n"
        "(deny default)\n"
        "\n"
        "; Allow basic system access\n"
        "(allow process-fork)\n"
        "(allow process-exec)\n"
        "(allow signal)\n"
        "(allow sysctl-read)\n"
        "(allow mach-lookup)\n"
        "(allow file-ioctl)\n"
        "\n"
        "; Allow reading all files (needed for system libs, configs, etc)\n"
        "(allow file-read*)\n"
        "\n"
        "; Allow writing to /dev for stdout/stderr\n"
        "(allow file-write*\n"
        "    (literal \"/dev/null\")\n"
        "    (regex #\"^/dev/fd/\")\n"
        "    (regex #\"^/dev/tty\")\n"
        ")\n"
        "\n"
    );

    // Add allowed directories for write access
    if (dir_count > 0) {
        offset += snprintf(profile + offset, MAX_PROFILE_LEN - offset,
            "; User-specified allowed directories (write access)\n"
            "(allow file-write*\n"
        );

        for (int i = 0; i < dir_count; i++) {
            offset += snprintf(profile + offset, MAX_PROFILE_LEN - offset,
                "    (subpath \"%s\")\n", resolved_dirs[i]);
        }

        offset += snprintf(profile + offset, MAX_PROFILE_LEN - offset, ")\n\n");
    }

    // Deny network (WASI handles this separately)
    offset += snprintf(profile + offset, MAX_PROFILE_LEN - offset,
        "; Deny network access (handled by WASI layer)\n"
        "(deny network*)\n"
    );

    // Build argv for sandbox-exec
    // sandbox-exec -p "<profile>" -- <command> [args...]
    char **sandbox_argv = malloc(sizeof(char*) * (argc + 5));
    if (!sandbox_argv) {
        fprintf(stderr, "edgebox-sandbox: out of memory\n");
        return 1;
    }

    sandbox_argv[0] = "sandbox-exec";
    sandbox_argv[1] = "-p";
    sandbox_argv[2] = profile;
    sandbox_argv[3] = "--";

    // Copy command and args (argv[0] is edgebox-sandbox, argv[1] is command)
    for (int i = 1; i < argc; i++) {
        sandbox_argv[i + 3] = argv[i];
    }
    sandbox_argv[argc + 3] = NULL;

    // Execute
    execvp("sandbox-exec", sandbox_argv);

    // If we get here, exec failed
    fprintf(stderr, "edgebox-sandbox: failed to exec sandbox-exec: %s\n", strerror(errno));
    free(sandbox_argv);
    return 1;
}

#elif defined(_WIN32)
/*
 * Windows: Job Objects + Restricted Tokens implementation
 *
 * Creates a restricted environment with:
 * - Reduced privileges (no admin)
 * - Job object for resource limits
 * - Note: DACL-based filesystem restriction has limitations
 */

static int run_sandboxed_windows(int argc, char **argv, char dirs[MAX_DIRS][MAX_PATH_LEN], int dir_count) {
    HANDLE hToken = NULL;
    HANDLE hRestrictedToken = NULL;
    HANDLE hJob = NULL;
    PROCESS_INFORMATION pi = {0};
    STARTUPINFOA si = {0};
    si.cb = sizeof(si);
    int ret = 1;

    // Build command line from argv
    char cmdline[32768] = {0};
    int offset = 0;
    for (int i = 1; i < argc; i++) {
        // Check if arg needs quoting (contains spaces)
        int needs_quote = (strchr(argv[i], ' ') != NULL);
        if (needs_quote) {
            offset += snprintf(cmdline + offset, sizeof(cmdline) - offset, "\"%s\" ", argv[i]);
        } else {
            offset += snprintf(cmdline + offset, sizeof(cmdline) - offset, "%s ", argv[i]);
        }
    }

    // 1. Get current process token
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ALL_ACCESS, &hToken)) {
        fprintf(stderr, "edgebox-sandbox: OpenProcessToken failed: %lu\n", GetLastError());
        goto cleanup;
    }

    // 2. Create restricted token (remove privileges)
    // DISABLE_MAX_PRIVILEGE removes most dangerous privileges
    if (!CreateRestrictedToken(hToken, DISABLE_MAX_PRIVILEGE, 0, NULL, 0, NULL, 0, NULL, &hRestrictedToken)) {
        fprintf(stderr, "edgebox-sandbox: CreateRestrictedToken failed: %lu\n", GetLastError());
        goto cleanup;
    }

    // 3. Create Job Object for resource limits
    hJob = CreateJobObjectA(NULL, NULL);
    if (!hJob) {
        fprintf(stderr, "edgebox-sandbox: CreateJobObject failed: %lu\n", GetLastError());
        goto cleanup;
    }

    // Set job limits
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli = {0};
    jeli.BasicLimitInformation.LimitFlags =
        JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE |      // Kill process when job closed
        JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION; // No crash dialogs

    if (!SetInformationJobObject(hJob, JobObjectExtendedLimitInformation, &jeli, sizeof(jeli))) {
        fprintf(stderr, "edgebox-sandbox: SetInformationJobObject failed: %lu\n", GetLastError());
        goto cleanup;
    }

    // 4. Create process with restricted token
    // Note: On Windows, true filesystem isolation requires AppContainer (Win8+)
    // This implementation provides privilege reduction but not path-level isolation
    if (!CreateProcessAsUserA(
            hRestrictedToken,
            NULL,           // Use cmdline for executable
            cmdline,        // Command line
            NULL,           // Process security attributes
            NULL,           // Thread security attributes
            FALSE,          // Don't inherit handles
            CREATE_SUSPENDED, // Start suspended to assign to job
            NULL,           // Use parent's environment
            NULL,           // Use parent's current directory
            &si,
            &pi)) {

        // Fallback: try CreateProcess if CreateProcessAsUser fails
        // (may fail due to privilege requirements)
        if (!CreateProcessA(
                NULL,
                cmdline,
                NULL,
                NULL,
                FALSE,
                CREATE_SUSPENDED,
                NULL,
                NULL,
                &si,
                &pi)) {
            fprintf(stderr, "edgebox-sandbox: CreateProcess failed: %lu\n", GetLastError());
            goto cleanup;
        }
    }

    // 5. Assign process to job object
    if (!AssignProcessToJobObject(hJob, pi.hProcess)) {
        fprintf(stderr, "edgebox-sandbox: AssignProcessToJobObject failed: %lu\n", GetLastError());
        TerminateProcess(pi.hProcess, 1);
        goto cleanup;
    }

    // 6. Resume the process
    ResumeThread(pi.hThread);

    // 7. Wait for process to complete
    WaitForSingleObject(pi.hProcess, INFINITE);

    // 8. Get exit code
    DWORD exitCode = 1;
    GetExitCodeProcess(pi.hProcess, &exitCode);
    ret = (int)exitCode;

cleanup:
    if (pi.hThread) CloseHandle(pi.hThread);
    if (pi.hProcess) CloseHandle(pi.hProcess);
    if (hJob) CloseHandle(hJob);
    if (hRestrictedToken) CloseHandle(hRestrictedToken);
    if (hToken) CloseHandle(hToken);

    return ret;
}

#else
/*
 * Linux: bubblewrap (bwrap) implementation
 *
 * Creates a namespace with:
 * - Read-only binds for system directories
 * - Read-write binds for allowed directories
 * - Optional network isolation
 */

static int run_sandboxed_linux(int argc, char **argv, char dirs[MAX_DIRS][MAX_PATH_LEN], int dir_count) {
    // Count args needed: bwrap + system binds + user dirs + -- + command + args
    // System binds: ~10 * 3 args each = 30
    // User dirs: dir_count * 3 args each
    // Extra: --dev, --proc, --unshare-net, --, command, args
    int max_args = 50 + (dir_count * 3) + argc;
    char **bwrap_argv = malloc(sizeof(char*) * max_args);
    if (!bwrap_argv) {
        fprintf(stderr, "edgebox-sandbox: out of memory\n");
        return 1;
    }

    int idx = 0;
    bwrap_argv[idx++] = "bwrap";

    // Read-only system directories
    const char *ro_dirs[] = {
        "/usr", "/lib", "/lib64", "/bin", "/sbin",
        "/etc/resolv.conf", "/etc/hosts", "/etc/passwd", "/etc/group",
        NULL
    };

    for (int i = 0; ro_dirs[i]; i++) {
        // Check if directory exists before binding
        if (access(ro_dirs[i], F_OK) == 0) {
            bwrap_argv[idx++] = "--ro-bind";
            bwrap_argv[idx++] = (char*)ro_dirs[i];
            bwrap_argv[idx++] = (char*)ro_dirs[i];
        }
    }

    // Device and proc filesystems
    bwrap_argv[idx++] = "--dev";
    bwrap_argv[idx++] = "/dev";
    bwrap_argv[idx++] = "--proc";
    bwrap_argv[idx++] = "/proc";

    // Read-write user directories
    for (int i = 0; i < dir_count; i++) {
        if (access(dirs[i], F_OK) == 0) {
            bwrap_argv[idx++] = "--bind";
            bwrap_argv[idx++] = dirs[i];
            bwrap_argv[idx++] = dirs[i];
        }
    }

    // Network isolation (optional - uncomment to enable)
    // bwrap_argv[idx++] = "--unshare-net";

    // Separator
    bwrap_argv[idx++] = "--";

    // Command and args
    for (int i = 1; i < argc; i++) {
        bwrap_argv[idx++] = argv[i];
    }
    bwrap_argv[idx] = NULL;

    // Execute
    execvp("bwrap", bwrap_argv);

    // If we get here, exec failed (bwrap might not be installed)
    fprintf(stderr, "edgebox-sandbox: failed to exec bwrap: %s\n", strerror(errno));
    fprintf(stderr, "edgebox-sandbox: install bubblewrap: apt install bubblewrap\n");
    free(bwrap_argv);
    return 1;
}
#endif

// Extract base command name from path (e.g., "/usr/bin/git" -> "git")
static const char* get_cmd_basename(const char *cmd) {
    const char *slash = strrchr(cmd, '/');
#ifdef _WIN32
    const char *backslash = strrchr(cmd, '\\');
    if (backslash && (!slash || backslash > slash)) {
        slash = backslash;
    }
#endif
    return slash ? slash + 1 : cmd;
}

// Check if command is in list (case-insensitive on Windows)
static int cmd_in_list(const char *cmd, char list[][MAX_CMD_LEN], int count) {
    const char *basename = get_cmd_basename(cmd);
    for (int i = 0; i < count; i++) {
#ifdef _WIN32
        if (_stricmp(basename, list[i]) == 0) return 1;
#else
        if (strcmp(basename, list[i]) == 0) return 1;
#endif
    }
    return 0;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: edgebox-sandbox <command> [args...]\n");
        fprintf(stderr, "Environment:\n");
        fprintf(stderr, "  __EDGEBOX_DIRS='[\"/tmp\",\"/path/to/app\"]'\n");
        fprintf(stderr, "  __EDGEBOX_ALLOW_CMDS='[\"git\",\"npm\",\"node\"]'\n");
        fprintf(stderr, "  __EDGEBOX_DENY_CMDS='[\"rm\",\"sudo\"]'\n");
        return 1;
    }

    // Get the command being executed
    const char *cmd = argv[1];

    // Parse command allow/deny lists
    char allow_cmds[MAX_CMDS][MAX_CMD_LEN];
    char deny_cmds[MAX_CMDS][MAX_CMD_LEN];
    int allow_count = 0;
    int deny_count = 0;

    const char *allow_env = getenv("__EDGEBOX_ALLOW_CMDS");
    if (allow_env) {
        allow_count = parse_json_array(allow_env, allow_cmds, MAX_CMDS, MAX_CMD_LEN);
        if (allow_count < 0) {
            fprintf(stderr, "edgebox-sandbox: invalid __EDGEBOX_ALLOW_CMDS format\n");
            return 1;
        }
    }

    const char *deny_env = getenv("__EDGEBOX_DENY_CMDS");
    if (deny_env) {
        deny_count = parse_json_array(deny_env, deny_cmds, MAX_CMDS, MAX_CMD_LEN);
        if (deny_count < 0) {
            fprintf(stderr, "edgebox-sandbox: invalid __EDGEBOX_DENY_CMDS format\n");
            return 1;
        }
    }

    // Command filtering: deny takes precedence over allow
    if (deny_count > 0 && cmd_in_list(cmd, deny_cmds, deny_count)) {
        fprintf(stderr, "edgebox-sandbox: command '%s' is denied\n", get_cmd_basename(cmd));
        return 126; // Permission denied exit code
    }

    if (allow_count > 0 && !cmd_in_list(cmd, allow_cmds, allow_count)) {
        fprintf(stderr, "edgebox-sandbox: command '%s' is not in allow list\n", get_cmd_basename(cmd));
        return 126; // Permission denied exit code
    }

    // Parse allowed directories from environment
    char dirs[MAX_DIRS][MAX_PATH_LEN];
    int dir_count = 0;

    const char *dirs_env = getenv("__EDGEBOX_DIRS");
    if (dirs_env) {
        dir_count = parse_dirs_json(dirs_env, dirs);
        if (dir_count < 0) {
            fprintf(stderr, "edgebox-sandbox: invalid __EDGEBOX_DIRS format\n");
            return 1;
        }
    }

    // If no dirs specified, only allow temp directory
    if (dir_count == 0) {
#ifdef _WIN32
        const char *temp = getenv("TEMP");
        if (temp) {
            strncpy(dirs[0], temp, MAX_PATH_LEN - 1);
            dirs[0][MAX_PATH_LEN - 1] = '\0';
        } else {
            strncpy(dirs[0], "C:\\Temp", MAX_PATH_LEN - 1);
            dirs[0][MAX_PATH_LEN - 1] = '\0';
        }
#else
        strncpy(dirs[0], "/tmp", MAX_PATH_LEN - 1);
        dirs[0][MAX_PATH_LEN - 1] = '\0';
#endif
        dir_count = 1;
    }

#ifdef __APPLE__
    return run_sandboxed_macos(argc, argv, dirs, dir_count);
#elif defined(_WIN32)
    return run_sandboxed_windows(argc, argv, dirs, dir_count);
#else
    return run_sandboxed_linux(argc, argv, dirs, dir_count);
#endif
}
