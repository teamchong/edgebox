// Comprehensive Math polyfill tests
// Tests all 31 Math functions + constants with edge cases

const {
    assertEqual, assertApprox, assertTrue, assertFalse,
    assertNaN, assertInfinity, assertNegInfinity, summary
} = require('./helpers/assert.js');

console.log('=== Math Polyfill Tests ===\n');

// ============================================
// Constants
// ============================================
console.log('--- Constants ---');
assertApprox(Math.PI, 3.141592653589793, 1e-15, 'Math.PI');
assertApprox(Math.E, 2.718281828459045, 1e-15, 'Math.E');
assertApprox(Math.LN2, 0.6931471805599453, 1e-15, 'Math.LN2');
assertApprox(Math.LN10, 2.302585092994046, 1e-15, 'Math.LN10');
assertApprox(Math.LOG2E, 1.4426950408889634, 1e-15, 'Math.LOG2E');
assertApprox(Math.LOG10E, 0.4342944819032518, 1e-15, 'Math.LOG10E');
assertApprox(Math.SQRT2, 1.4142135623730951, 1e-15, 'Math.SQRT2');
assertApprox(Math.SQRT1_2, 0.7071067811865476, 1e-15, 'Math.SQRT1_2');

// ============================================
// Math.abs - Absolute Value
// ============================================
console.log('\n--- Math.abs ---');
assertEqual(Math.abs(5), 5, 'Math.abs(5)');
assertEqual(Math.abs(-5), 5, 'Math.abs(-5)');
assertEqual(Math.abs(0), 0, 'Math.abs(0)');
assertEqual(Math.abs(-0), 0, 'Math.abs(-0)');
assertApprox(Math.abs(-3.14), 3.14, 1e-10, 'Math.abs(-3.14)');
assertApprox(Math.abs(3.14), 3.14, 1e-10, 'Math.abs(3.14)');
assertEqual(Math.abs(Infinity), Infinity, 'Math.abs(Infinity)');
assertEqual(Math.abs(-Infinity), Infinity, 'Math.abs(-Infinity)');
assertNaN(Math.abs(NaN), 'Math.abs(NaN)');
// Very small numbers
assertApprox(Math.abs(-1e-300), 1e-300, 1e-310, 'Math.abs(-1e-300)');
// Very large numbers
assertEqual(Math.abs(-1e308), 1e308, 'Math.abs(-1e308)');

// ============================================
// Math.sqrt - Square Root
// ============================================
console.log('\n--- Math.sqrt ---');
assertEqual(Math.sqrt(0), 0, 'Math.sqrt(0)');
assertEqual(Math.sqrt(1), 1, 'Math.sqrt(1)');
assertEqual(Math.sqrt(4), 2, 'Math.sqrt(4)');
assertEqual(Math.sqrt(9), 3, 'Math.sqrt(9)');
assertEqual(Math.sqrt(16), 4, 'Math.sqrt(16)');
assertEqual(Math.sqrt(25), 5, 'Math.sqrt(25)');
assertEqual(Math.sqrt(100), 10, 'Math.sqrt(100)');
assertApprox(Math.sqrt(2), 1.4142135623730951, 1e-15, 'Math.sqrt(2)');
assertApprox(Math.sqrt(3), 1.7320508075688772, 1e-15, 'Math.sqrt(3)');
assertNaN(Math.sqrt(-1), 'Math.sqrt(-1) is NaN');
assertNaN(Math.sqrt(-4), 'Math.sqrt(-4) is NaN');
assertEqual(Math.sqrt(Infinity), Infinity, 'Math.sqrt(Infinity)');
assertNaN(Math.sqrt(NaN), 'Math.sqrt(NaN)');
// Large numbers
assertApprox(Math.sqrt(1e10), 1e5, 1e-5, 'Math.sqrt(1e10)');
// Small numbers
assertApprox(Math.sqrt(1e-10), 1e-5, 1e-15, 'Math.sqrt(1e-10)');

// ============================================
// Math.cbrt - Cube Root
// ============================================
console.log('\n--- Math.cbrt ---');
assertEqual(Math.cbrt(0), 0, 'Math.cbrt(0)');
assertEqual(Math.cbrt(1), 1, 'Math.cbrt(1)');
assertApprox(Math.cbrt(8), 2, 1e-10, 'Math.cbrt(8)');
assertApprox(Math.cbrt(27), 3, 1e-10, 'Math.cbrt(27)');
assertApprox(Math.cbrt(64), 4, 1e-10, 'Math.cbrt(64)');
assertApprox(Math.cbrt(-8), -2, 1e-10, 'Math.cbrt(-8)');
assertApprox(Math.cbrt(-27), -3, 1e-10, 'Math.cbrt(-27)');
assertEqual(Math.cbrt(Infinity), Infinity, 'Math.cbrt(Infinity)');
assertEqual(Math.cbrt(-Infinity), -Infinity, 'Math.cbrt(-Infinity)');
assertNaN(Math.cbrt(NaN), 'Math.cbrt(NaN)');

// ============================================
// Math.floor - Floor
// ============================================
console.log('\n--- Math.floor ---');
assertEqual(Math.floor(3.7), 3, 'Math.floor(3.7)');
assertEqual(Math.floor(3.2), 3, 'Math.floor(3.2)');
assertEqual(Math.floor(3.0), 3, 'Math.floor(3.0)');
assertEqual(Math.floor(-3.7), -4, 'Math.floor(-3.7)');
assertEqual(Math.floor(-3.2), -4, 'Math.floor(-3.2)');
assertEqual(Math.floor(-3.0), -3, 'Math.floor(-3.0)');
assertEqual(Math.floor(0), 0, 'Math.floor(0)');
assertEqual(Math.floor(0.9999), 0, 'Math.floor(0.9999)');
assertEqual(Math.floor(-0.0001), -1, 'Math.floor(-0.0001)');
assertEqual(Math.floor(Infinity), Infinity, 'Math.floor(Infinity)');
assertEqual(Math.floor(-Infinity), -Infinity, 'Math.floor(-Infinity)');
assertNaN(Math.floor(NaN), 'Math.floor(NaN)');

// ============================================
// Math.ceil - Ceiling
// ============================================
console.log('\n--- Math.ceil ---');
assertEqual(Math.ceil(3.7), 4, 'Math.ceil(3.7)');
assertEqual(Math.ceil(3.2), 4, 'Math.ceil(3.2)');
assertEqual(Math.ceil(3.0), 3, 'Math.ceil(3.0)');
assertEqual(Math.ceil(-3.7), -3, 'Math.ceil(-3.7)');
assertEqual(Math.ceil(-3.2), -3, 'Math.ceil(-3.2)');
assertEqual(Math.ceil(-3.0), -3, 'Math.ceil(-3.0)');
assertEqual(Math.ceil(0), 0, 'Math.ceil(0)');
assertEqual(Math.ceil(0.0001), 1, 'Math.ceil(0.0001)');
assertEqual(Math.ceil(-0.9999), 0, 'Math.ceil(-0.9999)');
assertEqual(Math.ceil(Infinity), Infinity, 'Math.ceil(Infinity)');
assertEqual(Math.ceil(-Infinity), -Infinity, 'Math.ceil(-Infinity)');
assertNaN(Math.ceil(NaN), 'Math.ceil(NaN)');

// ============================================
// Math.round - Round
// ============================================
console.log('\n--- Math.round ---');
assertEqual(Math.round(3.5), 4, 'Math.round(3.5)');
assertEqual(Math.round(3.4), 3, 'Math.round(3.4)');
assertEqual(Math.round(3.6), 4, 'Math.round(3.6)');
assertEqual(Math.round(-3.5), -3, 'Math.round(-3.5)'); // Note: -3.5 rounds to -3, not -4
assertEqual(Math.round(-3.4), -3, 'Math.round(-3.4)');
assertEqual(Math.round(-3.6), -4, 'Math.round(-3.6)');
assertEqual(Math.round(0), 0, 'Math.round(0)');
assertEqual(Math.round(0.5), 1, 'Math.round(0.5)');
assertEqual(Math.round(-0.5), 0, 'Math.round(-0.5)'); // Note: -0.5 rounds to 0
assertEqual(Math.round(Infinity), Infinity, 'Math.round(Infinity)');
assertEqual(Math.round(-Infinity), -Infinity, 'Math.round(-Infinity)');
assertNaN(Math.round(NaN), 'Math.round(NaN)');

// ============================================
// Math.trunc - Truncate
// ============================================
console.log('\n--- Math.trunc ---');
assertEqual(Math.trunc(3.7), 3, 'Math.trunc(3.7)');
assertEqual(Math.trunc(3.2), 3, 'Math.trunc(3.2)');
assertEqual(Math.trunc(-3.7), -3, 'Math.trunc(-3.7)');
assertEqual(Math.trunc(-3.2), -3, 'Math.trunc(-3.2)');
assertEqual(Math.trunc(0), 0, 'Math.trunc(0)');
assertEqual(Math.trunc(0.9999), 0, 'Math.trunc(0.9999)');
assertEqual(Math.trunc(-0.9999), 0, 'Math.trunc(-0.9999)');
assertEqual(Math.trunc(Infinity), Infinity, 'Math.trunc(Infinity)');
assertEqual(Math.trunc(-Infinity), -Infinity, 'Math.trunc(-Infinity)');
assertNaN(Math.trunc(NaN), 'Math.trunc(NaN)');

// ============================================
// Math.sign - Sign
// ============================================
console.log('\n--- Math.sign ---');
assertEqual(Math.sign(5), 1, 'Math.sign(5)');
assertEqual(Math.sign(-5), -1, 'Math.sign(-5)');
assertEqual(Math.sign(0), 0, 'Math.sign(0)');
assertEqual(Math.sign(0.001), 1, 'Math.sign(0.001)');
assertEqual(Math.sign(-0.001), -1, 'Math.sign(-0.001)');
assertEqual(Math.sign(Infinity), 1, 'Math.sign(Infinity)');
assertEqual(Math.sign(-Infinity), -1, 'Math.sign(-Infinity)');
assertNaN(Math.sign(NaN), 'Math.sign(NaN)');

// ============================================
// Trigonometry - sin, cos, tan
// ============================================
console.log('\n--- Trigonometry (sin/cos/tan) ---');
assertApprox(Math.sin(0), 0, 1e-10, 'Math.sin(0)');
assertApprox(Math.sin(Math.PI / 6), 0.5, 1e-10, 'Math.sin(PI/6)');
assertApprox(Math.sin(Math.PI / 4), Math.SQRT2 / 2, 1e-10, 'Math.sin(PI/4)');
assertApprox(Math.sin(Math.PI / 2), 1, 1e-10, 'Math.sin(PI/2)');
assertApprox(Math.sin(Math.PI), 0, 1e-10, 'Math.sin(PI)');
assertApprox(Math.sin(-Math.PI / 2), -1, 1e-10, 'Math.sin(-PI/2)');

assertApprox(Math.cos(0), 1, 1e-10, 'Math.cos(0)');
assertApprox(Math.cos(Math.PI / 3), 0.5, 1e-10, 'Math.cos(PI/3)');
assertApprox(Math.cos(Math.PI / 4), Math.SQRT2 / 2, 1e-10, 'Math.cos(PI/4)');
assertApprox(Math.cos(Math.PI / 2), 0, 1e-10, 'Math.cos(PI/2)');
assertApprox(Math.cos(Math.PI), -1, 1e-10, 'Math.cos(PI)');

assertApprox(Math.tan(0), 0, 1e-10, 'Math.tan(0)');
assertApprox(Math.tan(Math.PI / 4), 1, 1e-10, 'Math.tan(PI/4)');
assertApprox(Math.tan(-Math.PI / 4), -1, 1e-10, 'Math.tan(-PI/4)');

// Edge cases
assertNaN(Math.sin(NaN), 'Math.sin(NaN)');
assertNaN(Math.cos(NaN), 'Math.cos(NaN)');
assertNaN(Math.tan(NaN), 'Math.tan(NaN)');
assertNaN(Math.sin(Infinity), 'Math.sin(Infinity)');
assertNaN(Math.cos(Infinity), 'Math.cos(Infinity)');
assertNaN(Math.tan(Infinity), 'Math.tan(Infinity)');

// ============================================
// Inverse Trigonometry - asin, acos, atan, atan2
// ============================================
console.log('\n--- Inverse Trigonometry ---');
assertApprox(Math.asin(0), 0, 1e-10, 'Math.asin(0)');
assertApprox(Math.asin(0.5), Math.PI / 6, 1e-10, 'Math.asin(0.5)');
assertApprox(Math.asin(1), Math.PI / 2, 1e-10, 'Math.asin(1)');
assertApprox(Math.asin(-1), -Math.PI / 2, 1e-10, 'Math.asin(-1)');
assertNaN(Math.asin(1.1), 'Math.asin(1.1) is NaN');
assertNaN(Math.asin(-1.1), 'Math.asin(-1.1) is NaN');

assertApprox(Math.acos(1), 0, 1e-10, 'Math.acos(1)');
assertApprox(Math.acos(0.5), Math.PI / 3, 1e-10, 'Math.acos(0.5)');
assertApprox(Math.acos(0), Math.PI / 2, 1e-10, 'Math.acos(0)');
assertApprox(Math.acos(-1), Math.PI, 1e-10, 'Math.acos(-1)');
assertNaN(Math.acos(1.1), 'Math.acos(1.1) is NaN');
assertNaN(Math.acos(-1.1), 'Math.acos(-1.1) is NaN');

assertApprox(Math.atan(0), 0, 1e-10, 'Math.atan(0)');
assertApprox(Math.atan(1), Math.PI / 4, 1e-10, 'Math.atan(1)');
assertApprox(Math.atan(-1), -Math.PI / 4, 1e-10, 'Math.atan(-1)');
assertApprox(Math.atan(Infinity), Math.PI / 2, 1e-10, 'Math.atan(Infinity)');
assertApprox(Math.atan(-Infinity), -Math.PI / 2, 1e-10, 'Math.atan(-Infinity)');

// atan2
assertApprox(Math.atan2(0, 1), 0, 1e-10, 'Math.atan2(0, 1)');
assertApprox(Math.atan2(1, 1), Math.PI / 4, 1e-10, 'Math.atan2(1, 1)');
assertApprox(Math.atan2(1, 0), Math.PI / 2, 1e-10, 'Math.atan2(1, 0)');
assertApprox(Math.atan2(1, -1), 3 * Math.PI / 4, 1e-10, 'Math.atan2(1, -1)');
assertApprox(Math.atan2(0, -1), Math.PI, 1e-10, 'Math.atan2(0, -1)');
assertApprox(Math.atan2(-1, -1), -3 * Math.PI / 4, 1e-10, 'Math.atan2(-1, -1)');
assertApprox(Math.atan2(-1, 0), -Math.PI / 2, 1e-10, 'Math.atan2(-1, 0)');
assertApprox(Math.atan2(-1, 1), -Math.PI / 4, 1e-10, 'Math.atan2(-1, 1)');

// ============================================
// Hyperbolic Functions
// ============================================
console.log('\n--- Hyperbolic Functions ---');
assertApprox(Math.sinh(0), 0, 1e-10, 'Math.sinh(0)');
assertApprox(Math.sinh(1), 1.1752011936438014, 1e-10, 'Math.sinh(1)');
assertApprox(Math.sinh(-1), -1.1752011936438014, 1e-10, 'Math.sinh(-1)');

assertApprox(Math.cosh(0), 1, 1e-10, 'Math.cosh(0)');
assertApprox(Math.cosh(1), 1.5430806348152437, 1e-10, 'Math.cosh(1)');
assertApprox(Math.cosh(-1), 1.5430806348152437, 1e-10, 'Math.cosh(-1)');

assertApprox(Math.tanh(0), 0, 1e-10, 'Math.tanh(0)');
assertApprox(Math.tanh(1), 0.7615941559557649, 1e-10, 'Math.tanh(1)');
assertApprox(Math.tanh(-1), -0.7615941559557649, 1e-10, 'Math.tanh(-1)');
assertApprox(Math.tanh(100), 1, 1e-10, 'Math.tanh(100) approaches 1');
assertApprox(Math.tanh(-100), -1, 1e-10, 'Math.tanh(-100) approaches -1');

// Inverse hyperbolic
assertApprox(Math.asinh(0), 0, 1e-10, 'Math.asinh(0)');
assertApprox(Math.asinh(1), 0.881373587019543, 1e-10, 'Math.asinh(1)');
assertApprox(Math.asinh(-1), -0.881373587019543, 1e-10, 'Math.asinh(-1)');

assertApprox(Math.acosh(1), 0, 1e-10, 'Math.acosh(1)');
assertApprox(Math.acosh(2), 1.3169578969248166, 1e-10, 'Math.acosh(2)');
assertNaN(Math.acosh(0.5), 'Math.acosh(0.5) is NaN');
assertNaN(Math.acosh(-1), 'Math.acosh(-1) is NaN');

assertApprox(Math.atanh(0), 0, 1e-10, 'Math.atanh(0)');
assertApprox(Math.atanh(0.5), 0.5493061443340548, 1e-10, 'Math.atanh(0.5)');
assertApprox(Math.atanh(-0.5), -0.5493061443340548, 1e-10, 'Math.atanh(-0.5)');
assertInfinity(Math.atanh(1), 'Math.atanh(1)');
assertNegInfinity(Math.atanh(-1), 'Math.atanh(-1)');
assertNaN(Math.atanh(1.5), 'Math.atanh(1.5) is NaN');

// ============================================
// Exponential and Logarithmic
// ============================================
console.log('\n--- Exponential/Logarithmic ---');
assertEqual(Math.exp(0), 1, 'Math.exp(0)');
assertApprox(Math.exp(1), Math.E, 1e-10, 'Math.exp(1)');
assertApprox(Math.exp(2), Math.E * Math.E, 1e-10, 'Math.exp(2)');
assertApprox(Math.exp(-1), 1 / Math.E, 1e-10, 'Math.exp(-1)');
assertEqual(Math.exp(Infinity), Infinity, 'Math.exp(Infinity)');
assertEqual(Math.exp(-Infinity), 0, 'Math.exp(-Infinity)');
assertNaN(Math.exp(NaN), 'Math.exp(NaN)');

assertApprox(Math.expm1(0), 0, 1e-10, 'Math.expm1(0)');
assertApprox(Math.expm1(1), Math.E - 1, 1e-10, 'Math.expm1(1)');
assertApprox(Math.expm1(-1), 1 / Math.E - 1, 1e-10, 'Math.expm1(-1)');

assertEqual(Math.log(1), 0, 'Math.log(1)');
assertApprox(Math.log(Math.E), 1, 1e-10, 'Math.log(E)');
assertApprox(Math.log(Math.E * Math.E), 2, 1e-10, 'Math.log(E^2)');
assertNegInfinity(Math.log(0), 'Math.log(0)');
assertNaN(Math.log(-1), 'Math.log(-1) is NaN');
assertEqual(Math.log(Infinity), Infinity, 'Math.log(Infinity)');

assertApprox(Math.log10(1), 0, 1e-10, 'Math.log10(1)');
assertApprox(Math.log10(10), 1, 1e-10, 'Math.log10(10)');
assertApprox(Math.log10(100), 2, 1e-10, 'Math.log10(100)');
assertApprox(Math.log10(1000), 3, 1e-10, 'Math.log10(1000)');

assertApprox(Math.log2(1), 0, 1e-10, 'Math.log2(1)');
assertApprox(Math.log2(2), 1, 1e-10, 'Math.log2(2)');
assertApprox(Math.log2(4), 2, 1e-10, 'Math.log2(4)');
assertApprox(Math.log2(8), 3, 1e-10, 'Math.log2(8)');
assertApprox(Math.log2(1024), 10, 1e-10, 'Math.log2(1024)');

assertApprox(Math.log1p(0), 0, 1e-10, 'Math.log1p(0)');
assertApprox(Math.log1p(1), Math.LN2, 1e-10, 'Math.log1p(1)');
assertApprox(Math.log1p(Math.E - 1), 1, 1e-10, 'Math.log1p(E-1)');
assertNegInfinity(Math.log1p(-1), 'Math.log1p(-1)');
assertNaN(Math.log1p(-2), 'Math.log1p(-2) is NaN');

// ============================================
// Power Functions
// ============================================
console.log('\n--- Power Functions ---');
assertEqual(Math.pow(2, 0), 1, 'Math.pow(2, 0)');
assertEqual(Math.pow(2, 1), 2, 'Math.pow(2, 1)');
assertEqual(Math.pow(2, 2), 4, 'Math.pow(2, 2)');
assertEqual(Math.pow(2, 3), 8, 'Math.pow(2, 3)');
assertEqual(Math.pow(2, 10), 1024, 'Math.pow(2, 10)');
assertApprox(Math.pow(2, -1), 0.5, 1e-10, 'Math.pow(2, -1)');
assertApprox(Math.pow(2, -2), 0.25, 1e-10, 'Math.pow(2, -2)');
assertApprox(Math.pow(2, 0.5), Math.SQRT2, 1e-10, 'Math.pow(2, 0.5)');
assertEqual(Math.pow(0, 0), 1, 'Math.pow(0, 0)'); // By convention
assertEqual(Math.pow(0, 1), 0, 'Math.pow(0, 1)');
assertEqual(Math.pow(-2, 2), 4, 'Math.pow(-2, 2)');
assertEqual(Math.pow(-2, 3), -8, 'Math.pow(-2, 3)');
assertNaN(Math.pow(-2, 0.5), 'Math.pow(-2, 0.5) is NaN');

// ============================================
// Math.hypot - Hypotenuse
// ============================================
console.log('\n--- Math.hypot ---');
assertEqual(Math.hypot(3, 4), 5, 'Math.hypot(3, 4)');
assertEqual(Math.hypot(5, 12), 13, 'Math.hypot(5, 12)');
assertApprox(Math.hypot(1, 1), Math.SQRT2, 1e-10, 'Math.hypot(1, 1)');
assertEqual(Math.hypot(0, 0), 0, 'Math.hypot(0, 0)');
assertEqual(Math.hypot(3), 3, 'Math.hypot(3)');
assertEqual(Math.hypot(-3), 3, 'Math.hypot(-3)');
// Multiple arguments
assertApprox(Math.hypot(3, 4, 0), 5, 1e-10, 'Math.hypot(3, 4, 0)');
assertApprox(Math.hypot(2, 3, 6), 7, 1e-10, 'Math.hypot(2, 3, 6)'); // sqrt(4+9+36) = 7
assertEqual(Math.hypot(), 0, 'Math.hypot()');
assertEqual(Math.hypot(Infinity, 1), Infinity, 'Math.hypot(Infinity, 1)');
assertEqual(Math.hypot(1, Infinity), Infinity, 'Math.hypot(1, Infinity)');
// NaN behavior
assertNaN(Math.hypot(NaN, 1), 'Math.hypot(NaN, 1)');

// ============================================
// Integer Operations - imul, clz32, fround
// ============================================
console.log('\n--- Integer Operations ---');
assertEqual(Math.imul(2, 4), 8, 'Math.imul(2, 4)');
assertEqual(Math.imul(3, 4), 12, 'Math.imul(3, 4)');
assertEqual(Math.imul(-1, 8), -8, 'Math.imul(-1, 8)');
assertEqual(Math.imul(-2, -2), 4, 'Math.imul(-2, -2)');
assertEqual(Math.imul(0xffffffff, 5), -5, 'Math.imul(0xffffffff, 5)');
// 32-bit overflow
assertEqual(Math.imul(0x80000000, 2), 0, 'Math.imul overflow');

assertEqual(Math.clz32(1), 31, 'Math.clz32(1)');
assertEqual(Math.clz32(2), 30, 'Math.clz32(2)');
assertEqual(Math.clz32(4), 29, 'Math.clz32(4)');
assertEqual(Math.clz32(0), 32, 'Math.clz32(0)');
assertEqual(Math.clz32(0x80000000), 0, 'Math.clz32(0x80000000)');
assertEqual(Math.clz32(0x7fffffff), 1, 'Math.clz32(0x7fffffff)');
assertEqual(Math.clz32(-1), 0, 'Math.clz32(-1)');

assertApprox(Math.fround(1.5), 1.5, 1e-10, 'Math.fround(1.5)');
assertApprox(Math.fround(1.337), 1.3370000123977661, 1e-7, 'Math.fround(1.337)');
assertEqual(Math.fround(Infinity), Infinity, 'Math.fround(Infinity)');
assertEqual(Math.fround(-Infinity), -Infinity, 'Math.fround(-Infinity)');
assertNaN(Math.fround(NaN), 'Math.fround(NaN)');

// ============================================
// Min/Max
// ============================================
console.log('\n--- Math.min/max ---');
assertEqual(Math.min(1, 2, 3), 1, 'Math.min(1, 2, 3)');
assertEqual(Math.min(3, 2, 1), 1, 'Math.min(3, 2, 1)');
assertEqual(Math.min(-1, -2, -3), -3, 'Math.min(-1, -2, -3)');
assertEqual(Math.min(0), 0, 'Math.min(0)');
assertEqual(Math.min(5), 5, 'Math.min(5)');
assertEqual(Math.min(), Infinity, 'Math.min()');
assertNaN(Math.min(1, NaN), 'Math.min(1, NaN)');
assertEqual(Math.min(Infinity, 0), 0, 'Math.min(Infinity, 0)');
assertEqual(Math.min(-Infinity, 0), -Infinity, 'Math.min(-Infinity, 0)');

assertEqual(Math.max(1, 2, 3), 3, 'Math.max(1, 2, 3)');
assertEqual(Math.max(3, 2, 1), 3, 'Math.max(3, 2, 1)');
assertEqual(Math.max(-1, -2, -3), -1, 'Math.max(-1, -2, -3)');
assertEqual(Math.max(0), 0, 'Math.max(0)');
assertEqual(Math.max(5), 5, 'Math.max(5)');
assertEqual(Math.max(), -Infinity, 'Math.max()');
assertNaN(Math.max(1, NaN), 'Math.max(1, NaN)');
assertEqual(Math.max(Infinity, 0), Infinity, 'Math.max(Infinity, 0)');
assertEqual(Math.max(-Infinity, 0), 0, 'Math.max(-Infinity, 0)');

// ============================================
// Math.random
// ============================================
console.log('\n--- Math.random ---');
let allInRange = true;
let hasVariation = false;
let lastRandom = null;
for (let i = 0; i < 100; i++) {
    const r = Math.random();
    if (r < 0 || r >= 1) {
        allInRange = false;
        break;
    }
    if (lastRandom !== null && r !== lastRandom) {
        hasVariation = true;
    }
    lastRandom = r;
}
assertTrue(allInRange, 'Math.random() always in [0, 1)');
assertTrue(hasVariation, 'Math.random() produces different values');

// ============================================
// Edge Cases - Mandelbrot-relevant
// ============================================
console.log('\n--- Mandelbrot Edge Cases ---');
// These are the exact patterns used in mandelbrot computation
const zr = -0.5;
const zi = 0.6;
const cr = -0.75;
const ci = 0.1;
const zr2 = zr * zr;
const zi2 = zi * zi;
const zrzi = zr * zi;

assertApprox(zr2, 0.25, 1e-10, 'zr*zr computation');
assertApprox(zi2, 0.36, 1e-10, 'zi*zi computation');
assertApprox(zr2 + zi2, 0.61, 1e-10, 'zr2 + zi2');
assertApprox(Math.sqrt(zr2 + zi2), 0.7810249675906654, 1e-10, 'sqrt(zr2 + zi2)');
assertApprox(zr2 - zi2 + cr, -0.86, 1e-10, 'new zr = zr2 - zi2 + cr');
assertApprox(2 * zrzi + ci, -0.5, 1e-10, 'new zi = 2*zr*zi + ci');

// Test the escape radius check
const escapeRadius = 4;
const magnitude = zr2 + zi2;
assertTrue(magnitude < escapeRadius, 'magnitude < escape radius');

summary();
