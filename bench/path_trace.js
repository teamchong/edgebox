// Path Tracer Benchmark
// Tests: floating-point math, vector operations, recursion
// Simple ray tracer with spheres and diffuse lighting

var log = typeof print === "function" ? print : console.log;

// Vector operations (inline for performance)
function vecAdd(a, b) { return [a[0] + b[0], a[1] + b[1], a[2] + b[2]]; }
function vecSub(a, b) { return [a[0] - b[0], a[1] - b[1], a[2] - b[2]]; }
function vecMul(a, s) { return [a[0] * s, a[1] * s, a[2] * s]; }
function vecDot(a, b) { return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]; }
function vecLen(a) { return Math.sqrt(vecDot(a, a)); }
function vecNorm(a) { var l = vecLen(a); return [a[0] / l, a[1] / l, a[2] / l]; }

// Spheres: [center_x, center_y, center_z, radius, color_r, color_g, color_b]
var spheres = [
    [0, -1, 3, 1, 1, 0, 0],      // Red sphere
    [2, 0, 4, 1, 0, 1, 0],       // Green sphere
    [-2, 0, 4, 1, 0, 0, 1],      // Blue sphere
    [0, -5001, 0, 5000, 1, 1, 0] // Yellow ground
];

function intersectSphere(origin, dir, sphere) {
    var center = [sphere[0], sphere[1], sphere[2]];
    var radius = sphere[3];
    var oc = vecSub(origin, center);
    var a = vecDot(dir, dir);
    var b = 2.0 * vecDot(oc, dir);
    var c = vecDot(oc, oc) - radius * radius;
    var disc = b * b - 4 * a * c;
    if (disc < 0) return -1;
    return (-b - Math.sqrt(disc)) / (2 * a);
}

function traceRay(origin, dir, tMin, tMax) {
    var closestT = Infinity;
    var closestSphere = null;

    for (var i = 0; i < spheres.length; i++) {
        var t = intersectSphere(origin, dir, spheres[i]);
        if (t > tMin && t < tMax && t < closestT) {
            closestT = t;
            closestSphere = spheres[i];
        }
    }

    if (closestSphere === null) {
        return [0.2, 0.2, 0.2];  // Background color
    }

    // Compute lighting
    var hitPoint = vecAdd(origin, vecMul(dir, closestT));
    var normal = vecNorm(vecSub(hitPoint, [closestSphere[0], closestSphere[1], closestSphere[2]]));
    var lightDir = vecNorm([1, 1, -1]);
    var intensity = Math.max(0, vecDot(normal, lightDir)) * 0.8 + 0.2;  // Diffuse + ambient

    return [
        closestSphere[4] * intensity,
        closestSphere[5] * intensity,
        closestSphere[6] * intensity
    ];
}

function render(width, height) {
    var checksum = 0;
    var origin = [0, 0, 0];

    for (var y = 0; y < height; y++) {
        for (var x = 0; x < width; x++) {
            // Camera ray
            var dx = (x - width / 2) / width;
            var dy = (height / 2 - y) / height;
            var dir = vecNorm([dx, dy, 1]);

            var color = traceRay(origin, dir, 0.001, 1000);
            checksum += Math.floor(color[0] * 255);
            checksum += Math.floor(color[1] * 255);
            checksum += Math.floor(color[2] * 255);
        }
    }
    return checksum;
}

var WIDTH = 100;
var HEIGHT = 100;
var RUNS = 20;
var EXPECTED = 1785434;  // Pre-computed checksum

var times = [];
var result;

for (var i = 0; i < RUNS; i++) {
    var start = performance.now();
    result = render(WIDTH, HEIGHT);
    times.push(performance.now() - start);
}

var total = 0;
for (var i = 0; i < times.length; i++) total += times[i];
var avg = total / times.length;

if (result !== EXPECTED) {
    log("FAIL: path_trace checksum = " + result + ", expected " + EXPECTED);
} else {
    log(EXPECTED + " (" + avg.toFixed(2) + "ms avg, " + times.map(function(t) { return t.toFixed(0); }).join("/") + ")");
}
