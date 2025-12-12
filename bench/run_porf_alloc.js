const fs = require('fs');
const wasm = fs.readFileSync(__dirname + '/alloc_stress_porf.wasm');

const imports = {
    '': {
        b: (x) => process.stdout.write(String(x)),
        c: (x) => process.stdout.write(String.fromCharCode(x))
    }
};

WebAssembly.instantiate(wasm, imports).then(({ instance }) => {
    instance.exports.m();
});
