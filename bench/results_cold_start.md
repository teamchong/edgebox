| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/Downloads/repos/edgebox/edgebox-aot.dylib /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 14.4 ± 0.5 | 13.4 | 15.7 | 1.00 |
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/.wasmedge/lib/wasmedge_quickjs_aot.wasm /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 17.6 ± 1.6 | 15.8 | 22.7 | 1.22 ± 0.12 |
| `/Users/steven_chong/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 100.1 ± 3.5 | 97.1 | 109.7 | 6.96 ± 0.35 |
| `node /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 32.2 ± 1.4 | 31.0 | 37.3 | 2.24 ± 0.13 |
| `bun /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 14.6 ± 0.6 | 13.7 | 16.1 | 1.01 ± 0.06 |
