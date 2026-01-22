// Comprehensive RxJS test - tests the complete callback issue with real RxJS
const { Observable, of, from, Subject, BehaviorSubject, ReplaySubject, interval, timer, EMPTY } = require('rxjs');
const { take, map, filter, tap, finalize, first, last, single, takeUntil, takeWhile } = require('rxjs/operators');

let testNum = 0;
let passed = 0;
let failed = 0;
let pending = 0;

function test(name, fn) {
    testNum++;
    const testId = testNum;
    try {
        const result = fn();
        if (result instanceof Promise) {
            pending++;
            result.then(() => {
                passed++;
                pending--;
                console.log(`✓ Test ${testId}: ${name}`);
                checkDone();
            }).catch(e => {
                failed++;
                pending--;
                console.log(`✗ Test ${testId}: ${name} - ${e.message}`);
                checkDone();
            });
        } else {
            console.log(`✓ Test ${testId}: ${name}`);
            passed++;
        }
    } catch (e) {
        console.log(`✗ Test ${testId}: ${name} - ${e.message}`);
        failed++;
    }
}

function assertEqual(actual, expected, msg) {
    if (actual !== expected) {
        throw new Error(`${msg}: expected ${expected}, got ${actual}`);
    }
}

function checkDone() {
    if (pending === 0) {
        console.log("\n=== Final Summary ===");
        console.log(`Passed: ${passed}/${testNum}`);
        console.log(`Failed: ${failed}/${testNum}`);
        if (failed > 0) {
            console.log("\nFAIL: Some tests failed");
        } else {
            console.log("\nPASS: All tests passed!");
        }
    }
}

console.log("=== RxJS Comprehensive Tests ===\n");

// Test 1: Basic of() with function form
test("of() - function form complete fires", () => {
    let completed = false;
    of(1, 2, 3).subscribe(
        v => {},
        e => {},
        () => { completed = true; }
    );
    assertEqual(completed, true, "complete should fire");
});

// Test 2: of() with object form - THE CRITICAL TEST
test("of() - object form complete fires", () => {
    let completed = false;
    of(1, 2, 3).subscribe({
        next: v => {},
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 3: from() with object form
test("from() - object form complete fires", () => {
    let completed = false;
    from([1, 2, 3]).subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 4: EMPTY observable with object form
test("EMPTY - object form complete fires immediately", () => {
    let completed = false;
    EMPTY.subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 5: Observable.create with object form
test("Observable.create - object form complete fires", () => {
    let completed = false;
    new Observable(subscriber => {
        subscriber.next(1);
        subscriber.complete();
    }).subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 6: Subject with object form
test("Subject - object form complete fires", () => {
    let completed = false;
    const subject = new Subject();
    subject.subscribe({
        complete: () => { completed = true; }
    });
    subject.complete();
    assertEqual(completed, true, "complete should fire");
});

// Test 7: BehaviorSubject with object form
test("BehaviorSubject - object form complete fires", () => {
    let completed = false;
    const subject = new BehaviorSubject(0);
    subject.subscribe({
        complete: () => { completed = true; }
    });
    subject.complete();
    assertEqual(completed, true, "complete should fire");
});

// Test 8: ReplaySubject with object form
test("ReplaySubject - object form complete fires", () => {
    let completed = false;
    const subject = new ReplaySubject(3);
    subject.subscribe({
        complete: () => { completed = true; }
    });
    subject.complete();
    assertEqual(completed, true, "complete should fire");
});

// Test 9: take() operator with object form
test("take(3) - object form complete fires", () => {
    let completed = false;
    of(1, 2, 3, 4, 5).pipe(take(3)).subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 10: first() operator with object form
test("first() - object form complete fires", () => {
    let completed = false;
    of(1, 2, 3).pipe(first()).subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 11: last() operator with object form
test("last() - object form complete fires", () => {
    let completed = false;
    of(1, 2, 3).pipe(last()).subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 12: single() operator with object form
test("single() - object form complete fires", () => {
    let completed = false;
    of(42).pipe(single()).subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 13: filter() + take() with object form
test("filter().take() - object form complete fires", () => {
    let completed = false;
    of(1, 2, 3, 4, 5, 6).pipe(
        filter(x => x % 2 === 0),
        take(2)
    ).subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 14: map() with object form
test("map() - object form complete fires", () => {
    let completed = false;
    of(1, 2, 3).pipe(map(x => x * 2)).subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 15: tap() + finalize() with object form
test("tap().finalize() - object form complete fires", () => {
    let completed = false;
    let finalized = false;
    of(1, 2, 3).pipe(
        tap(x => {}),
        finalize(() => { finalized = true; })
    ).subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
    assertEqual(finalized, true, "finalize should fire");
});

// Test 16: Multiple subscribers with object form
test("Multiple subscribers - all complete callbacks fire", () => {
    let completed1 = false;
    let completed2 = false;
    const source = of(1, 2, 3);
    source.subscribe({ complete: () => { completed1 = true; } });
    source.subscribe({ complete: () => { completed2 = true; } });
    assertEqual(completed1, true, "first subscriber complete should fire");
    assertEqual(completed2, true, "second subscriber complete should fire");
});

// Test 17: Nested subscribe with object form
test("Nested subscribe - inner complete fires", () => {
    let completed = false;
    of(1).subscribe({
        next: () => {
            of(2).subscribe({
                complete: () => { completed = true; }
            });
        }
    });
    assertEqual(completed, true, "inner complete should fire");
});

// Test 18: takeWhile with object form
test("takeWhile() - object form complete fires", () => {
    let completed = false;
    of(1, 2, 3, 4, 5).pipe(takeWhile(x => x < 4)).subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 19: Arrow function in object with closure
test("Arrow function with closure - complete fires", () => {
    let completed = false;
    const outerVar = "test";
    of(1).subscribe({
        complete: () => {
            // Closure over outerVar
            const _ = outerVar;
            completed = true;
        }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 20: Complete with side effects
test("Complete with side effects", () => {
    let values = [];
    let completed = false;
    of(1, 2, 3).subscribe({
        next: v => values.push(v),
        complete: () => {
            values.push('done');
            completed = true;
        }
    });
    assertEqual(values.join(','), '1,2,3,done', "values should include done");
    assertEqual(completed, true, "complete should fire");
});

// Test 21: Error doesn't trigger complete
test("Error path - complete should NOT fire", () => {
    let completed = false;
    let errored = false;
    new Observable(subscriber => {
        subscriber.next(1);
        subscriber.error(new Error("test error"));
    }).subscribe({
        error: () => { errored = true; },
        complete: () => { completed = true; }
    });
    assertEqual(errored, true, "error should fire");
    assertEqual(completed, false, "complete should NOT fire");
});

// Test 22: Unsubscribe before complete
test("Unsubscribe before complete - complete should NOT fire", () => {
    let completed = false;
    const subject = new Subject();
    const sub = subject.subscribe({
        complete: () => { completed = true; }
    });
    sub.unsubscribe();
    subject.complete();
    assertEqual(completed, false, "complete should NOT fire after unsubscribe");
});

// Test 23: Complete only fires once
test("Complete only fires once", () => {
    let completeCount = 0;
    const subject = new Subject();
    subject.subscribe({
        complete: () => { completeCount++; }
    });
    subject.complete();
    subject.complete();
    subject.complete();
    assertEqual(completeCount, 1, "complete should only fire once");
});

// Test 24: Method shorthand in observer
test("Method shorthand in observer - complete fires", () => {
    let completed = false;
    of(1, 2, 3).subscribe({
        next(v) {},
        complete() { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 25: Complete with undefined next
test("Observer with only complete - complete fires", () => {
    let completed = false;
    of(1, 2, 3).subscribe({
        complete: () => { completed = true; }
    });
    assertEqual(completed, true, "complete should fire");
});

// Test 26: Async complete with timer
test("timer(10).take(1) - async complete fires", () => {
    return new Promise((resolve, reject) => {
        let completed = false;
        timer(10).pipe(take(1)).subscribe({
            complete: () => {
                completed = true;
                if (completed) resolve();
                else reject(new Error("complete should fire"));
            }
        });
        setTimeout(() => {
            if (!completed) reject(new Error("complete not fired in time"));
        }, 100);
    });
});

// Test 27: interval with take - async complete
test("interval(5).take(3) - async complete fires", () => {
    return new Promise((resolve, reject) => {
        let completed = false;
        let count = 0;
        interval(5).pipe(take(3)).subscribe({
            next: () => { count++; },
            complete: () => {
                completed = true;
                if (completed && count === 3) resolve();
                else reject(new Error(`complete=${completed}, count=${count}`));
            }
        });
        setTimeout(() => {
            if (!completed) reject(new Error("complete not fired in time"));
        }, 200);
    });
});

// Initial summary (before async tests complete)
setTimeout(() => {
    console.log("\n=== Initial Summary (sync tests) ===");
    console.log(`Passed: ${passed}/${testNum}`);
    console.log(`Failed: ${failed}/${testNum}`);
    console.log(`Pending: ${pending}`);
}, 0);
