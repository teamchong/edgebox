| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/Downloads/repos/edgebox/edgebox-aot.dylib /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 17.5 ± 0.5 | 16.9 | 18.2 | 1.18 ± 0.06 |
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/.wasmedge/lib/wasmedge_quickjs_aot.wasm /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 17.1 ± 0.4 | 16.6 | 17.7 | 1.16 ± 0.05 |
| `/Users/steven_chong/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 99.5 ± 1.2 | 98.6 | 101.5 | 6.73 ± 0.28 |
| `node /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 32.0 ± 0.6 | 31.4 | 32.7 | 2.17 ± 0.09 |
| `bun /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 14.8 ± 0.6 | 14.2 | 15.7 | 1.00 |
