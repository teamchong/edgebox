#!/usr/bin/env bash
set -e

# Regenerate QuickJS patch from scratch
# This script:
# 1. Resets QuickJS submodule to clean state
# 2. Manually applies all frozen interpreter changes
# 3. Generates a clean patch file

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
QUICKJS_DIR="$REPO_ROOT/vendor/quickjs-ng"
PATCH_FILE="$REPO_ROOT/patches/001-frozen-interpreter-all.patch"

echo "[1/3] Resetting QuickJS to clean state..."
cd "$QUICKJS_DIR"
git reset --hard HEAD
git clean -fd

echo "[2/3] Applying frozen interpreter changes..."

# Change 1: quickjs-libc.c - Remove exit(1) on promise rejection
cat > /tmp/quickjs-libc-patch.txt << 'EOF'
--- a/quickjs-libc.c
+++ b/quickjs-libc.c
@@ -4507,11 +4507,14 @@ static void js_std_promise_rejection_check(JSContext *ctx)
     if (unlikely(!list_empty(&ts->rejected_promise_list))) {
         list_for_each(el, &ts->rejected_promise_list) {
             JSRejectedPromiseEntry *rp = list_entry(el, JSRejectedPromiseEntry, link);
+#ifndef __wasi__
+            /* Only print rejection warnings on non-WASI platforms */
             fprintf(stderr, "Possibly unhandled promise rejection: ");
             js_std_dump_error1(ctx, rp->reason);
             fflush(stderr);
+#endif
         }
-        exit(1);
+        /* Don't exit - allow the event loop to continue or exit naturally */
     }
 }
EOF

patch -p1 < /tmp/quickjs-libc-patch.txt

echo "[3/3] Generating clean patch..."
git add -A
git diff --cached > "$PATCH_FILE"

echo "✅ Patch regenerated successfully: $PATCH_FILE"
echo "Lines: $(wc -l < "$PATCH_FILE")"
