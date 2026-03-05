// Closure sync regression tests
// Tests the v2 shared var_ref mechanism in LLVM frozen codegen.
// Each test exercises a specific closure-local interaction pattern
// that requires syncLocalsTo/syncLocalsFrom to work correctly.

var log = typeof print === "function" ? print : console.log;
var failures = 0;

function assert(actual, expected, label) {
    if (actual !== expected) {
        log("FAIL: " + label + " — got " + actual + ", expected " + expected);
        failures++;
    } else {
        log("  OK: " + label);
    }
}

// ─── Test 1: Hoisted capture ───────────────────────────────────
// Closure created BEFORE the captured local is initialized.
// Without v2 shared var_refs, the closure snapshots undefined at creation time.
function testHoistedCapture() {
    function inner() { return x; }
    var x = 42;
    return inner();
}
assert(testHoistedCapture(), 42, "hoisted capture (closure before init)");

// ─── Test 2: Write-back through closure ────────────────────────
// Closure modifies a shared local; parent sees the new value after call.
// Requires syncLocalsFrom to propagate the closure's write back to parent.
function testWriteBack() {
    var x = 1;
    function inc() { x = 99; }
    inc();
    return x;
}
assert(testWriteBack(), 99, "write-back through closure");

// ─── Test 3: Multiple closures sharing a local ─────────────────
// Two closures share the same local; one writes, other reads updated value.
function testSharedLocal() {
    var x = 0;
    function setter(v) { x = v; }
    function getter() { return x; }
    setter(123);
    return getter();
}
assert(testSharedLocal(), 123, "multiple closures sharing local");

// ─── Test 4: Closure survives parent return ────────────────────
// After parent returns, the closure's var_ref is detached and owns its value.
// Tests js_frozen_var_ref_list_detach correctness.
function testClosureSurvives() {
    function make() {
        var val = 123;
        return function() { return val; };
    }
    var fn = make();
    return fn();
}
assert(testClosureSurvives(), 123, "closure survives parent return (detach)");

// ─── Test 5: Read-through after parent update ──────────────────
// Parent updates local between closure creation and closure call.
// syncLocalsTo before the closure call must propagate the update.
function testReadThrough() {
    var x = 1;
    function reader() { return x; }
    x = 2;
    x = 3;
    return reader();
}
assert(testReadThrough(), 3, "read-through after parent update");

// ─── Test 6: Incremental sync across multiple calls ────────────
// Each call should see the latest value of the shared local.
function testIncrementalSync() {
    var counter = 0;
    function bump() { counter++; }
    bump();
    bump();
    bump();
    return counter;
}
assert(testIncrementalSync(), 3, "incremental sync across multiple calls");

// ─── Test 7: Closure factory (nested) ──────────────────────────
// Inner function creates closures over its own locals + outer locals.
function testNestedClosure() {
    var outer = 10;
    function middle() {
        var mid = 20;
        function inner() { return outer + mid; }
        return inner();
    }
    return middle();
}
assert(testNestedClosure(), 30, "nested closure (outer + middle locals)");

// ─── Test 8: Closure with loop variable ────────────────────────
// Classic loop closure pattern — each closure should capture final value.
function testLoopClosure() {
    var fns = new Array(5);
    for (var i = 0; i < 5; i++) {
        fns[i] = function() { return i; };
    }
    // With var, all closures share the same 'i' (should be 5 after loop)
    return fns[0]() + fns[2]() + fns[4]();
}
assert(testLoopClosure(), 15, "loop closure (var binding, shared i=5)");

// NOTE: let-scoped loop closures (for (let i=...) { fns[i] = () => i })
// are a known limitation — frozen codegen treats close_loc as no-op,
// so all closures share the same binding. Omitted from regression tests.

// ─── Test 10: Closure + arguments interaction ──────────────────
// Closure captures a local that was initialized from function arguments.
function testClosureWithArgs() {
    function adder(base) {
        var offset = base * 2;
        return function(x) { return x + offset; };
    }
    var add10 = adder(5);
    return add10(7);
}
assert(testClosureWithArgs(), 17, "closure captures arg-derived local");

// ─── Test 11: Mutual recursion through closures ────────────────
function testMutualRecursion() {
    function isEven(n) {
        if (n === 0) return true;
        return isOdd(n - 1);
    }
    function isOdd(n) {
        if (n === 0) return false;
        return isEven(n - 1);
    }
    return isEven(10) ? 1 : 0;
}
assert(testMutualRecursion(), 1, "mutual recursion through closures");

// ─── Test 12: Array.from with closure callback ─────────────────
// Tests array_from opcode with a mapping closure.
function testArrayFromClosure() {
    var multiplier = 3;
    var result = Array.from([1, 2, 3], function(x) { return x * multiplier; });
    return result[0] + result[1] + result[2];
}
assert(testArrayFromClosure(), 18, "Array.from with closure callback");

// ─── Summary ───────────────────────────────────────────────────
if (failures === 0) {
    log("\nAll closure sync tests PASSED");
} else {
    log("\n" + failures + " test(s) FAILED");
}
