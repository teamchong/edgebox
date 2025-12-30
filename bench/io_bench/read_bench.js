// IO Benchmark - File Read Performance
// Tests reading a file multiple times

var fs = require('fs');

var FILE_PATH = '/tmp/io_bench_test.txt';
var ITERATIONS = 1000;

// Warmup
for (var i = 0; i < 10; i++) {
    fs.readFileSync(FILE_PATH, 'utf8');
}

var start = Date.now();
for (var i = 0; i < ITERATIONS; i++) {
    fs.readFileSync(FILE_PATH, 'utf8');
}
var end = Date.now();

var elapsed = end - start;
var ops_per_sec = Math.round((ITERATIONS / elapsed) * 1000);

console.log('read_ops_per_sec:' + ops_per_sec);
