const fs = require('fs');
const wasm = fs.readFileSync(__dirname + '/fib_porf.wasm');

const imports = {
    '': {
        a: () => {},
        b: (x) => {},
        c: (x) => {}
    }
};

const RUNS = 10;

WebAssembly.instantiate(wasm, imports).then(({ instance }) => {
    const times = [];
    for (let i = 0; i < RUNS; i++) {
        const start = performance.now();
        instance.exports.m();
        times.push(performance.now() - start);
    }
    const avg = times.reduce((a, b) => a + b, 0) / times.length;
    console.log(`1134903170 (${avg.toFixed(2)}ms avg, ${times.map(t => t.toFixed(0)).join('/')})`);
});
