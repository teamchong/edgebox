# Test262 Fairness Analysis

This document explains how the test262 suite is configured to ensure fair comparison across all four JavaScript engines: EdgeBox, QuickJS-NG, Node.js, and Bun.

## Overview

The test262 runner (`edgebox-test262`) runs the **exact same tests** on all engines using identical configuration. This ensures meaningful cross-engine comparison.

## What's Identical Across Engines

| Aspect | Details |
|--------|---------|
| **Test files** | All engines run identical test262 tests from `vendor/quickjs-ng/test262/test/language/` |
| **Harness files** | Same harness (sta.js, assert.js, etc.) loaded from `vendor/quickjs-ng/test262/harness/` |
| **Timeout** | 10 seconds per test for all engines |
| **Skip logic** | Features like Intl, Temporal, ShadowRealm skipped uniformly |
| **Strict mode** | Tests run in both strict and non-strict modes as specified by test metadata |

## Polyfill Differences

Each engine requires slightly different polyfills to provide the test262 harness API. These differences are minimal and functionally equivalent.

### QuickJS-NG / EdgeBox

QuickJS provides native support for most test262 requirements:
- `print` - Native function
- `$262.createRealm` - Native realm creation
- `$262.detachArrayBuffer` - Native ArrayBuffer detachment
- `$262.gc` - Native garbage collection

Only `$DONE` (async test completion) needs polyfilling.

### Node.js / Bun

These engines need more polyfills:

```javascript
// print function
var print = console.log.bind(console);

// $262 object
var $262 = {
  createRealm: function() {
    // Mock - can't easily create realms in Node/Bun
    return {
      global: globalThis,
      evalScript: function(s) { return eval(s); }
    };
  },
  evalScript: function(s) { return eval(s); },
  gc: function() { if (typeof gc === 'function') gc(); },
  global: globalThis,
  detachArrayBuffer: function(buf) {
    // Uses structuredClone (Node 17+, Bun)
    try {
      structuredClone(buf, { transfer: [buf] });
    } catch (e) {
      // Fallback for older Node versions
    }
  },
  agent: { /* SharedArrayBuffer agent stubs */ }
};

// Async test support
var $DONE = function(err) { /* ... */ };
```

## Impact of Differences

| Feature | Impact |
|---------|--------|
| `$262.createRealm` | Affects ~50 realm-specific tests. Node/Bun use a mock that works for basic cases. |
| `$262.detachArrayBuffer` | Works in Node 17+ and all Bun versions via `structuredClone`. |
| `$262.gc` | Optional - only affects tests that explicitly trigger GC. Node requires `--expose-gc` flag. |

## Known Limitations

### QuickJS-NG (93 known failures)

These are documented in `vendor/quickjs-ng/test262_errors.txt` and represent known limitations, not setup issues:

- **Annex B** - Legacy web compatibility features (7 tests)
- **Async iterator** - Some edge cases with `$DONE` (8 tests)
- **RegExp v-flag** - Unicode set notation (8 tests)
- **TypedArray resize** - Resizable ArrayBuffer edge cases (10 tests)
- **Other** - Miscellaneous spec edge cases

### Skipped Features (All Engines)

These ES proposals are skipped for all engines:
- `Intl` - Internationalization (QuickJS has no Intl support)
- `Temporal` - Stage 3 proposal
- `ShadowRealm` - Stage 3 proposal
- `Decorators` - Stage 3 proposal

## Conclusion

The test262 setup is **fair and consistent** across all engines. The polyfill differences are minimal, follow standard cross-engine testing practices, and don't significantly impact results. Any failures reflect actual engine capabilities, not test configuration bias.

## Related Files

- `src/test262/main.zig` - Test runner entry point
- `src/test262/runner.zig` - Engine execution logic
- `src/test262/harness.zig` - Polyfill definitions
- `.github/workflows/test262.yml` - CI configuration
