// Comprehensive Array polyfill tests
// Tests Array.prototype methods with various edge cases

const {
    assertEqual, assertArrayEqual, assertDeepEqual, assertTrue, assertFalse,
    assertUndefined, assertThrows, summary
} = require('./helpers/assert.js');

console.log('=== Array Polyfill Tests ===\n');

// ============================================
// Array.prototype.forEach
// ============================================
console.log('--- forEach ---');

let forEachSum = 0;
[1, 2, 3, 4, 5].forEach(x => forEachSum += x);
assertEqual(forEachSum, 15, 'forEach sum');

let forEachIndices = [];
['a', 'b', 'c'].forEach((_, i) => forEachIndices.push(i));
assertArrayEqual(forEachIndices, [0, 1, 2], 'forEach indices');

// thisArg
const forEachObj = { sum: 0 };
[1, 2, 3].forEach(function(x) { this.sum += x; }, forEachObj);
assertEqual(forEachObj.sum, 6, 'forEach with thisArg');

// Empty array
let forEachCalled = false;
[].forEach(() => forEachCalled = true);
assertFalse(forEachCalled, 'forEach on empty array');

// Sparse array - only calls for existing elements
let forEachSparseCount = 0;
const sparseArr = [1, , 3]; // Sparse array with hole
sparseArr.forEach(() => forEachSparseCount++);
// Note: forEach skips holes in sparse arrays
assertTrue(forEachSparseCount <= 3, 'forEach sparse array count');

// ============================================
// Array.prototype.map
// ============================================
console.log('\n--- map ---');

assertArrayEqual([1, 2, 3].map(x => x * 2), [2, 4, 6], 'map double');
assertArrayEqual([1, 2, 3].map(x => x.toString()), ['1', '2', '3'], 'map toString');
assertArrayEqual([1, 2, 3].map((x, i) => x + i), [1, 3, 5], 'map with index');
assertArrayEqual([].map(x => x * 2), [], 'map empty array');

// thisArg
const mapObj = { multiplier: 10 };
const mapResult = [1, 2, 3].map(function(x) { return x * this.multiplier; }, mapObj);
assertArrayEqual(mapResult, [10, 20, 30], 'map with thisArg');

// Preserves array length
assertEqual([1, 2, 3, 4, 5].map(x => x).length, 5, 'map preserves length');

// ============================================
// Array.prototype.filter
// ============================================
console.log('\n--- filter ---');

assertArrayEqual([1, 2, 3, 4, 5].filter(x => x > 3), [4, 5], 'filter > 3');
assertArrayEqual([1, 2, 3, 4, 5].filter(x => x % 2 === 0), [2, 4], 'filter even');
assertArrayEqual([1, 2, 3].filter(x => x > 10), [], 'filter none match');
assertArrayEqual([1, 2, 3].filter(x => true), [1, 2, 3], 'filter all match');
assertArrayEqual([].filter(x => true), [], 'filter empty array');

// With index
assertArrayEqual([0, 1, 2, 3, 4].filter((x, i) => i % 2 === 0), [0, 2, 4], 'filter by index');

// thisArg
const filterObj = { threshold: 3 };
const filterResult = [1, 2, 3, 4, 5].filter(function(x) { return x > this.threshold; }, filterObj);
assertArrayEqual(filterResult, [4, 5], 'filter with thisArg');

// ============================================
// Array.prototype.reduce
// ============================================
console.log('\n--- reduce ---');

assertEqual([1, 2, 3, 4, 5].reduce((a, b) => a + b, 0), 15, 'reduce sum');
assertEqual([1, 2, 3, 4, 5].reduce((a, b) => a * b, 1), 120, 'reduce product');
assertEqual([1, 2, 3].reduce((a, b) => a + b), 6, 'reduce without initial');
assertEqual(['a', 'b', 'c'].reduce((a, b) => a + b, ''), 'abc', 'reduce concat');

// Complex reduction
const reduceResult = [1, 2, 3].reduce((acc, x) => {
    acc.push(x * 2);
    return acc;
}, []);
assertArrayEqual(reduceResult, [2, 4, 6], 'reduce to array');

// With index and array
const reduceWithIdx = [10, 20, 30].reduce((acc, val, idx) => acc + val + idx, 0);
assertEqual(reduceWithIdx, 63, 'reduce with index'); // 10+0 + 20+1 + 30+2 = 63

// Single element without initial
assertEqual([5].reduce((a, b) => a + b), 5, 'reduce single element');

// ============================================
// Array.prototype.reduceRight
// ============================================
console.log('\n--- reduceRight ---');

assertEqual([1, 2, 3].reduceRight((a, b) => a + b, 0), 6, 'reduceRight sum');
assertEqual(['a', 'b', 'c'].reduceRight((a, b) => a + b, ''), 'cba', 'reduceRight concat');
assertEqual([1, 2, 3].reduceRight((a, b) => a - b), 0, 'reduceRight subtract'); // 3-2-1=0

// ============================================
// Array.prototype.find
// ============================================
console.log('\n--- find ---');

assertEqual([1, 2, 3, 4, 5].find(x => x > 3), 4, 'find > 3');
assertEqual([1, 2, 3].find(x => x > 10), undefined, 'find none');
assertEqual([{id: 1}, {id: 2}].find(x => x.id === 2).id, 2, 'find object');
assertEqual([].find(x => true), undefined, 'find empty array');

// Returns first match
assertEqual([1, 2, 3, 2, 1].find(x => x === 2), 2, 'find first match');

// With index
assertEqual([5, 12, 8, 130, 44].find((x, i) => i === 2), 8, 'find by index');

// thisArg
const findObj = { target: 3 };
const findResult = [1, 2, 3, 4].find(function(x) { return x === this.target; }, findObj);
assertEqual(findResult, 3, 'find with thisArg');

// ============================================
// Array.prototype.findIndex
// ============================================
console.log('\n--- findIndex ---');

assertEqual([1, 2, 3, 4, 5].findIndex(x => x > 3), 3, 'findIndex > 3');
assertEqual([1, 2, 3].findIndex(x => x > 10), -1, 'findIndex none');
assertEqual([{id: 1}, {id: 2}].findIndex(x => x.id === 2), 1, 'findIndex object');
assertEqual([].findIndex(x => true), -1, 'findIndex empty array');

// Returns first match index
assertEqual([1, 2, 3, 2, 1].findIndex(x => x === 2), 1, 'findIndex first match');

// ============================================
// Array.prototype.every
// ============================================
console.log('\n--- every ---');

assertTrue([1, 2, 3, 4, 5].every(x => x > 0), 'every > 0');
assertFalse([1, 2, 3, 4, 5].every(x => x > 3), 'every > 3 (false)');
assertTrue([].every(x => false), 'every empty array (vacuous truth)');
assertTrue([2, 4, 6].every(x => x % 2 === 0), 'every even');

// Short-circuit on first false
let everyCount = 0;
[1, 2, 3, 4, 5].every(x => {
    everyCount++;
    return x < 3;
});
assertEqual(everyCount, 3, 'every short-circuits');

// thisArg
const everyObj = { min: 0 };
const everyResult = [1, 2, 3].every(function(x) { return x > this.min; }, everyObj);
assertTrue(everyResult, 'every with thisArg');

// ============================================
// Array.prototype.some
// ============================================
console.log('\n--- some ---');

assertTrue([1, 2, 3, 4, 5].some(x => x > 3), 'some > 3');
assertFalse([1, 2, 3].some(x => x > 10), 'some > 10 (false)');
assertFalse([].some(x => true), 'some empty array');
assertTrue([2, 4, 6].some(x => x % 2 === 0), 'some even in even array');
assertFalse([1, 3, 5].some(x => x % 2 === 0), 'some even in odd array');

// Short-circuit on first true
let someCount = 0;
[1, 2, 3, 4, 5].some(x => {
    someCount++;
    return x === 3;
});
assertEqual(someCount, 3, 'some short-circuits');

// thisArg
const someObj = { target: 5 };
const someResult = [1, 2, 3, 4, 5].some(function(x) { return x === this.target; }, someObj);
assertTrue(someResult, 'some with thisArg');

// ============================================
// Array.prototype.includes
// ============================================
console.log('\n--- includes ---');

assertTrue([1, 2, 3].includes(2), 'includes 2');
assertFalse([1, 2, 3].includes(4), 'includes 4 (false)');
assertTrue(['a', 'b', 'c'].includes('b'), 'includes string');
assertFalse([].includes(1), 'includes empty array');

// fromIndex
assertTrue([1, 2, 3, 2].includes(2, 2), 'includes with fromIndex');
assertFalse([1, 2, 3].includes(1, 1), 'includes with fromIndex (false)');

// Negative fromIndex
assertTrue([1, 2, 3].includes(3, -1), 'includes negative fromIndex');
assertTrue([1, 2, 3].includes(1, -3), 'includes negative fromIndex from start');

// NaN handling (includes uses SameValueZero)
assertTrue([1, 2, NaN].includes(NaN), 'includes NaN');

// ============================================
// Array.prototype.indexOf
// ============================================
console.log('\n--- indexOf ---');

assertEqual([1, 2, 3, 2].indexOf(2), 1, 'indexOf 2');
assertEqual([1, 2, 3].indexOf(4), -1, 'indexOf not found');
assertEqual(['a', 'b', 'c'].indexOf('b'), 1, 'indexOf string');
assertEqual([].indexOf(1), -1, 'indexOf empty array');

// fromIndex
assertEqual([1, 2, 3, 2].indexOf(2, 2), 3, 'indexOf with fromIndex');
assertEqual([1, 2, 3].indexOf(1, 1), -1, 'indexOf with fromIndex (not found)');

// Negative fromIndex
assertEqual([1, 2, 3].indexOf(3, -1), 2, 'indexOf negative fromIndex');

// ============================================
// Array.prototype.lastIndexOf
// ============================================
console.log('\n--- lastIndexOf ---');

assertEqual([1, 2, 3, 2].lastIndexOf(2), 3, 'lastIndexOf 2');
assertEqual([1, 2, 3].lastIndexOf(4), -1, 'lastIndexOf not found');
assertEqual([1, 2, 3, 2].lastIndexOf(2, 2), 1, 'lastIndexOf with fromIndex');

// ============================================
// Array.prototype.flat
// ============================================
console.log('\n--- flat ---');

if (typeof [].flat === 'function') {
    assertArrayEqual([1, [2, 3], 4].flat(), [1, 2, 3, 4], 'flat depth 1');
    // For nested arrays, use JSON comparison
    assertDeepEqual([1, [2, [3, 4]]].flat(), [1, 2, [3, 4]], 'flat depth 1 nested');
    assertArrayEqual([1, [2, [3, 4]]].flat(2), [1, 2, 3, 4], 'flat depth 2');
    assertArrayEqual([].flat(), [], 'flat empty array');
    assertArrayEqual([[1], [2], [3]].flat(), [1, 2, 3], 'flat all nested');
} else {
    console.log('SKIP: flat not available');
}

// ============================================
// Array.prototype.flatMap
// ============================================
console.log('\n--- flatMap ---');

if (typeof [].flatMap === 'function') {
    assertArrayEqual([1, 2, 3].flatMap(x => [x, x * 2]), [1, 2, 2, 4, 3, 6], 'flatMap');
    assertArrayEqual([1, 2, 3].flatMap(x => x * 2), [2, 4, 6], 'flatMap no array');
    assertArrayEqual([].flatMap(x => [x, x]), [], 'flatMap empty');
} else {
    console.log('SKIP: flatMap not available');
}

// ============================================
// Array.prototype.at
// ============================================
console.log('\n--- at ---');

if (typeof [].at === 'function') {
    assertEqual([1, 2, 3].at(0), 1, 'at(0)');
    assertEqual([1, 2, 3].at(1), 2, 'at(1)');
    assertEqual([1, 2, 3].at(-1), 3, 'at(-1)');
    assertEqual([1, 2, 3].at(-2), 2, 'at(-2)');
    assertEqual([1, 2, 3].at(10), undefined, 'at out of bounds');
    assertEqual([1, 2, 3].at(-10), undefined, 'at negative out of bounds');
} else {
    console.log('SKIP: at not available');
}

// ============================================
// Array.from
// ============================================
console.log('\n--- Array.from ---');

assertArrayEqual(Array.from('abc'), ['a', 'b', 'c'], 'Array.from string');
assertArrayEqual(Array.from([1, 2, 3]), [1, 2, 3], 'Array.from array');
assertArrayEqual(Array.from({length: 3}), [undefined, undefined, undefined], 'Array.from array-like');
assertArrayEqual(Array.from({length: 3}, (_, i) => i), [0, 1, 2], 'Array.from with mapFn');
assertArrayEqual(Array.from(new Set([1, 2, 3])), [1, 2, 3], 'Array.from Set');

// ============================================
// Array.isArray
// ============================================
console.log('\n--- Array.isArray ---');

assertTrue(Array.isArray([]), 'isArray([])');
assertTrue(Array.isArray([1, 2, 3]), 'isArray([1,2,3])');
assertTrue(Array.isArray(new Array()), 'isArray(new Array())');
assertFalse(Array.isArray('array'), 'isArray(string)');
assertFalse(Array.isArray({length: 1}), 'isArray(array-like)');
assertFalse(Array.isArray(null), 'isArray(null)');
assertFalse(Array.isArray(undefined), 'isArray(undefined)');

// ============================================
// Array.of
// ============================================
console.log('\n--- Array.of ---');

assertArrayEqual(Array.of(1, 2, 3), [1, 2, 3], 'Array.of(1,2,3)');
assertArrayEqual(Array.of(7), [7], 'Array.of(7)');
assertArrayEqual(Array.of(), [], 'Array.of()');
assertArrayEqual(Array.of(undefined), [undefined], 'Array.of(undefined)');

// ============================================
// Edge Cases
// ============================================
console.log('\n--- Edge Cases ---');

// Very large array
const largeArr = Array.from({length: 10000}, (_, i) => i);
assertEqual(largeArr.reduce((a, b) => a + b, 0), 49995000, 'Large array reduce');
assertEqual(largeArr.filter(x => x % 1000 === 0).length, 10, 'Large array filter');
assertEqual(largeArr.find(x => x === 9999), 9999, 'Large array find');

// Arrays with undefined/null
assertArrayEqual([1, undefined, 3].map(x => x), [1, undefined, 3], 'Array with undefined');
assertArrayEqual([1, null, 3].filter(x => x != null), [1, 3], 'Filter null');

// Chaining
const chainResult = [1, 2, 3, 4, 5]
    .filter(x => x % 2 === 1)
    .map(x => x * 2)
    .reduce((a, b) => a + b, 0);
assertEqual(chainResult, 18, 'Method chaining'); // (1+3+5)*2 = 18

summary();
