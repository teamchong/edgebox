const fs = require('fs');
const wasm = fs.readFileSync(__dirname + '/fib_porf.wasm');

const imports = {
    '': {
        b: (x) => process.stdout.write(String(x)),
        c: (x) => process.stdout.write(String.fromCharCode(x))
    }
};

WebAssembly.instantiate(wasm, imports).then(({ instance }) => {
    const start = performance.now();
    instance.exports.m();
    const elapsed = performance.now() - start;
    console.log(' (' + elapsed.toFixed(2) + 'ms)');
});
