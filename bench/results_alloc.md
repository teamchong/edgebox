| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `/Users/steven_chong/Downloads/repos/edgebox/zig-out/bin/edgebox /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.wasm` | 41.2 ± 0.4 | 40.7 | 41.6 | 2.35 ± 0.18 |
| `bun /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 17.6 ± 1.3 | 16.1 | 18.7 | 1.00 |
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/.wasmedge/lib/wasmedge_quickjs_aot.wasm /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 34.6 ± 0.5 | 34.0 | 34.9 | 1.97 ± 0.15 |
| `node /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 34.3 ± 0.8 | 33.4 | 35.0 | 1.95 ± 0.16 |
| `porffor /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 379.4 ± 7.5 | 370.9 | 384.9 | 21.61 ± 1.69 |
