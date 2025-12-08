| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/Downloads/repos/edgebox/edgebox-aot.dylib /Users/steven_chong/Downloads/repos/edgebox/bench/fib.js` | 124.347 | 124.347 | 124.347 | 28.04 |
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/.wasmedge/lib/wasmedge_quickjs_aot.wasm /Users/steven_chong/Downloads/repos/edgebox/bench/fib.js` | 148.880 | 148.880 | 148.880 | 33.57 |
| `/Users/steven_chong/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf /Users/steven_chong/Downloads/repos/edgebox/bench/fib.js` | 7.563 | 7.563 | 7.563 | 1.71 |
| `node /Users/steven_chong/Downloads/repos/edgebox/bench/fib.js` | 6.449 | 6.449 | 6.449 | 1.45 |
| `bun /Users/steven_chong/Downloads/repos/edgebox/bench/fib.js` | 4.435 | 4.435 | 4.435 | 1.00 |
