| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/Downloads/repos/edgebox/edgebox-aot.dylib /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 42.1 ± 0.7 | 41.4 | 42.8 | 2.14 ± 0.06 |
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/.wasmedge/lib/wasmedge_quickjs_aot.wasm /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 34.9 ± 0.7 | 34.3 | 35.7 | 1.78 ± 0.05 |
| `/Users/steven_chong/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 292.6 ± 2.4 | 290.3 | 295.0 | 14.88 ± 0.35 |
| `node /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 35.9 ± 0.5 | 35.6 | 36.5 | 1.82 ± 0.05 |
| `bun /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 19.7 ± 0.4 | 19.3 | 20.1 | 1.00 |
