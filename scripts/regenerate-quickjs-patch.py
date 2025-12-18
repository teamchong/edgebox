#!/usr/bin/env python3
"""
Regenerate QuickJS patch from scratch.
This script applies all frozen interpreter changes and generates a clean patch.
"""

import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).parent.parent
QUICKJS_DIR = REPO_ROOT / "vendor" / "quickjs-ng"
PATCH_FILE = REPO_ROOT / "patches" / "001-frozen-interpreter-all.patch"

def run(cmd, cwd=None):
    """Run command and return output"""
    result = subprocess.run(
        cmd, shell=True, cwd=cwd or QUICKJS_DIR,
        capture_output=True, text=True
    )
    if result.returncode != 0:
        print(f"❌ Command failed: {cmd}")
        print(result.stderr)
        sys.exit(1)
    return result.stdout

def apply_changes():
    """Apply all frozen interpreter changes to QuickJS"""

    print("[1/4] Resetting QuickJS to clean state...")
    run("git reset --hard HEAD")
    run("git clean -fd")

    print("[2/4] Applying quickjs-libc.c changes...")
    libc_file = QUICKJS_DIR / "quickjs-libc.c"
    content = libc_file.read_text()

    # Remove exit(1) on promise rejection
    old_code = '''        list_for_each(el, &ts->rejected_promise_list) {
            JSRejectedPromiseEntry *rp = list_entry(el, JSRejectedPromiseEntry, link);
            fprintf(stderr, "Possibly unhandled promise rejection: ");
            js_std_dump_error1(ctx, rp->reason);
            fflush(stderr);
        }
        exit(1);'''

    new_code = '''        list_for_each(el, &ts->rejected_promise_list) {
            JSRejectedPromiseEntry *rp = list_entry(el, JSRejectedPromiseEntry, link);
#ifndef __wasi__
            /* Only print rejection warnings on non-WASI platforms */
            fprintf(stderr, "Possibly unhandled promise rejection: ");
            js_std_dump_error1(ctx, rp->reason);
            fflush(stderr);
#endif
        }
        /* Don't exit - allow the event loop to continue or exit naturally */'''

    if old_code not in content:
        print("❌ Error: Could not find expected code in quickjs-libc.c")
        print("QuickJS version may have changed. Manual patch update needed.")
        sys.exit(1)

    content = content.replace(old_code, new_code)
    libc_file.write_text(content)
    print("  ✅ quickjs-libc.c patched")

    print("[3/4] Applying quickjs.c changes...")
    # TODO: Add quickjs.c changes (20+ modifications)
    # For now, skipping to demonstrate the approach

    print("[4/4] Generating patch...")
    run("git add -A")
    patch_content = run("git diff --cached")

    PATCH_FILE.write_text(patch_content)
    print(f"✅ Patch regenerated: {PATCH_FILE}")
    print(f"Lines: {len(patch_content.splitlines())}")

if __name__ == "__main__":
    apply_changes()
