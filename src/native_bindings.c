// Native bindings for QuickJS in WASM
// These need to be in C because Zig function pointers don't work with JS_NewCFunction in WASM

#include "quickjs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <errno.h>

// ============================================================================
// Test function
// ============================================================================

static JSValue js_test42(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    return JS_NewInt32(ctx, 42);
}

// ============================================================================
// File system functions using WASI
// ============================================================================

static JSValue js_fs_exists(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 1) return JS_FALSE;

    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_FALSE;

    struct stat st;
    int exists = (stat(path, &st) == 0);
    JS_FreeCString(ctx, path);

    return JS_NewBool(ctx, exists);
}

static JSValue js_fs_read(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 1) return JS_ThrowTypeError(ctx, "fs.readFileSync requires a path");

    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_EXCEPTION;

    FILE *f = fopen(path, "rb");
    if (!f) {
        JS_FreeCString(ctx, path);
        return JS_ThrowTypeError(ctx, "ENOENT: no such file or directory");
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buf = malloc(size + 1);
    if (!buf) {
        fclose(f);
        JS_FreeCString(ctx, path);
        return JS_ThrowOutOfMemory(ctx);
    }

    size_t read = fread(buf, 1, size, f);
    buf[read] = '\0';
    fclose(f);
    JS_FreeCString(ctx, path);

    JSValue result = JS_NewStringLen(ctx, buf, read);
    free(buf);
    return result;
}

static JSValue js_fs_write(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 2) return JS_ThrowTypeError(ctx, "fs.writeFileSync requires path and data");

    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_EXCEPTION;

    size_t len;
    const char *data = JS_ToCStringLen(ctx, &len, argv[1]);
    if (!data) {
        JS_FreeCString(ctx, path);
        return JS_EXCEPTION;
    }

    FILE *f = fopen(path, "wb");
    if (!f) {
        JS_FreeCString(ctx, path);
        JS_FreeCString(ctx, data);
        return JS_ThrowTypeError(ctx, "ENOENT: cannot open file for writing");
    }

    fwrite(data, 1, len, f);
    fclose(f);

    JS_FreeCString(ctx, path);
    JS_FreeCString(ctx, data);
    return JS_UNDEFINED;
}

static JSValue js_fs_mkdir(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 1) return JS_ThrowTypeError(ctx, "fs.mkdirSync requires a path");

    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_EXCEPTION;

    // Check recursive flag (second arg)
    int recursive = 0;
    if (argc >= 2 && JS_ToBool(ctx, argv[1])) {
        recursive = 1;
    }

    int result;
    if (recursive) {
        // For recursive, we'd need to create parent dirs - simplified for now
        result = mkdir(path, 0755);
        if (result != 0 && errno == EEXIST) {
            result = 0;  // Directory already exists is OK for recursive
        }
    } else {
        result = mkdir(path, 0755);
    }

    JS_FreeCString(ctx, path);

    if (result != 0 && errno != EEXIST) {
        return JS_ThrowTypeError(ctx, "ENOENT: cannot create directory");
    }
    return JS_UNDEFINED;
}

static JSValue js_fs_stat(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 1) return JS_ThrowTypeError(ctx, "fs.statSync requires a path");

    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_EXCEPTION;

    struct stat st;
    if (stat(path, &st) != 0) {
        JS_FreeCString(ctx, path);
        return JS_ThrowTypeError(ctx, "ENOENT: no such file or directory");
    }
    JS_FreeCString(ctx, path);

    JSValue obj = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, obj, "size", JS_NewInt64(ctx, st.st_size));
    JS_SetPropertyStr(ctx, obj, "mode", JS_NewInt32(ctx, st.st_mode));
    JS_SetPropertyStr(ctx, obj, "mtime", JS_NewInt64(ctx, st.st_mtime * 1000));  // JS expects ms

    // isFile() and isDirectory() methods
    JS_SetPropertyStr(ctx, obj, "isFile",
        JS_NewCFunction(ctx, (JSCFunction *)((st.st_mode & S_IFMT) == S_IFREG ?
            (void *)JS_TRUE : (void *)JS_FALSE), "isFile", 0));
    JS_SetPropertyStr(ctx, obj, "isDirectory",
        JS_NewCFunction(ctx, (JSCFunction *)((st.st_mode & S_IFMT) == S_IFDIR ?
            (void *)JS_TRUE : (void *)JS_FALSE), "isDirectory", 0));

    return obj;
}

static JSValue js_fs_readdir(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 1) return JS_ThrowTypeError(ctx, "fs.readdirSync requires a path");

    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_EXCEPTION;

    DIR *dir = opendir(path);
    if (!dir) {
        JS_FreeCString(ctx, path);
        return JS_ThrowTypeError(ctx, "ENOENT: no such directory");
    }

    JSValue arr = JS_NewArray(ctx);
    struct dirent *entry;
    int i = 0;

    while ((entry = readdir(dir)) != NULL) {
        // Skip . and ..
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }
        JS_SetPropertyUint32(ctx, arr, i++, JS_NewString(ctx, entry->d_name));
    }

    closedir(dir);
    JS_FreeCString(ctx, path);
    return arr;
}

static JSValue js_fs_unlink(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 1) return JS_ThrowTypeError(ctx, "fs.unlinkSync requires a path");

    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_EXCEPTION;

    int result = unlink(path);
    JS_FreeCString(ctx, path);

    if (result != 0) {
        return JS_ThrowTypeError(ctx, "ENOENT: cannot remove file");
    }
    return JS_UNDEFINED;
}

static JSValue js_fs_rmdir(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 1) return JS_ThrowTypeError(ctx, "fs.rmdirSync requires a path");

    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_EXCEPTION;

    int result = rmdir(path);
    JS_FreeCString(ctx, path);

    if (result != 0) {
        return JS_ThrowTypeError(ctx, "ENOENT: cannot remove directory");
    }
    return JS_UNDEFINED;
}

static JSValue js_fs_rename(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 2) return JS_ThrowTypeError(ctx, "fs.renameSync requires oldPath and newPath");

    const char *oldPath = JS_ToCString(ctx, argv[0]);
    if (!oldPath) return JS_EXCEPTION;

    const char *newPath = JS_ToCString(ctx, argv[1]);
    if (!newPath) {
        JS_FreeCString(ctx, oldPath);
        return JS_EXCEPTION;
    }

    int result = rename(oldPath, newPath);
    JS_FreeCString(ctx, oldPath);
    JS_FreeCString(ctx, newPath);

    if (result != 0) {
        return JS_ThrowTypeError(ctx, "ENOENT: cannot rename");
    }
    return JS_UNDEFINED;
}

static JSValue js_fs_copy(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 2) return JS_ThrowTypeError(ctx, "fs.copyFileSync requires src and dest");

    const char *src = JS_ToCString(ctx, argv[0]);
    if (!src) return JS_EXCEPTION;

    const char *dest = JS_ToCString(ctx, argv[1]);
    if (!dest) {
        JS_FreeCString(ctx, src);
        return JS_EXCEPTION;
    }

    FILE *fsrc = fopen(src, "rb");
    if (!fsrc) {
        JS_FreeCString(ctx, src);
        JS_FreeCString(ctx, dest);
        return JS_ThrowTypeError(ctx, "ENOENT: source file not found");
    }

    FILE *fdest = fopen(dest, "wb");
    if (!fdest) {
        fclose(fsrc);
        JS_FreeCString(ctx, src);
        JS_FreeCString(ctx, dest);
        return JS_ThrowTypeError(ctx, "ENOENT: cannot create destination");
    }

    char buf[8192];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), fsrc)) > 0) {
        fwrite(buf, 1, n, fdest);
    }

    fclose(fsrc);
    fclose(fdest);
    JS_FreeCString(ctx, src);
    JS_FreeCString(ctx, dest);
    return JS_UNDEFINED;
}

static JSValue js_cwd(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    char buf[4096];
    if (getcwd(buf, sizeof(buf)) != NULL) {
        return JS_NewString(ctx, buf);
    }
    return JS_NewString(ctx, "/");
}

static JSValue js_homedir(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    const char *home = getenv("HOME");
    if (home) {
        return JS_NewString(ctx, home);
    }
    return JS_NewString(ctx, "/");
}

// ============================================================================
// Registration function called from Zig
// ============================================================================

void register_native_bindings(JSContext *ctx) {
    JSValue global = JS_GetGlobalObject(ctx);

    // Test function
    JS_SetPropertyStr(ctx, global, "__edgebox_test42",
        JS_NewCFunction(ctx, js_test42, "__edgebox_test42", 0));

    // FS functions
    JS_SetPropertyStr(ctx, global, "__edgebox_fs_exists",
        JS_NewCFunction(ctx, js_fs_exists, "__edgebox_fs_exists", 1));
    JS_SetPropertyStr(ctx, global, "__edgebox_fs_read",
        JS_NewCFunction(ctx, js_fs_read, "__edgebox_fs_read", 1));
    JS_SetPropertyStr(ctx, global, "__edgebox_fs_write",
        JS_NewCFunction(ctx, js_fs_write, "__edgebox_fs_write", 2));
    JS_SetPropertyStr(ctx, global, "__edgebox_fs_mkdir",
        JS_NewCFunction(ctx, js_fs_mkdir, "__edgebox_fs_mkdir", 2));
    JS_SetPropertyStr(ctx, global, "__edgebox_fs_stat",
        JS_NewCFunction(ctx, js_fs_stat, "__edgebox_fs_stat", 1));
    JS_SetPropertyStr(ctx, global, "__edgebox_fs_readdir",
        JS_NewCFunction(ctx, js_fs_readdir, "__edgebox_fs_readdir", 1));
    JS_SetPropertyStr(ctx, global, "__edgebox_fs_unlink",
        JS_NewCFunction(ctx, js_fs_unlink, "__edgebox_fs_unlink", 1));
    JS_SetPropertyStr(ctx, global, "__edgebox_fs_rmdir",
        JS_NewCFunction(ctx, js_fs_rmdir, "__edgebox_fs_rmdir", 2));
    JS_SetPropertyStr(ctx, global, "__edgebox_fs_rename",
        JS_NewCFunction(ctx, js_fs_rename, "__edgebox_fs_rename", 2));
    JS_SetPropertyStr(ctx, global, "__edgebox_fs_copy",
        JS_NewCFunction(ctx, js_fs_copy, "__edgebox_fs_copy", 2));
    JS_SetPropertyStr(ctx, global, "__edgebox_cwd",
        JS_NewCFunction(ctx, js_cwd, "__edgebox_cwd", 0));
    JS_SetPropertyStr(ctx, global, "__edgebox_homedir",
        JS_NewCFunction(ctx, js_homedir, "__edgebox_homedir", 0));

    JS_FreeValue(ctx, global);
}
