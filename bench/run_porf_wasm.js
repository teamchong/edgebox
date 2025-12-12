const fs = require('fs');
const wasm = fs.readFileSync(__dirname + '/fib_porf.wasm');

const imports = {
    '': {
        b: (x) => process.stdout.write(String(x)),
        c: (x) => process.stdout.write(String.fromCharCode(x))
    }
};

// fib.js already has the 10x loop with performance.now()
// Just run the WASM once
WebAssembly.instantiate(wasm, imports).then(({ instance }) => {
    instance.exports.m();
});
