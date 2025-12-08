| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `/Users/steven_chong/Downloads/repos/edgebox/run.sh /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 185.1 ± 79.7 | 98.7 | 357.3 | 7.86 ± 3.92 |
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/.wasmedge/lib/wasmedge_quickjs_aot.wasm /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 39.8 ± 1.4 | 38.1 | 43.2 | 1.69 ± 0.43 |
| `node /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 80.8 ± 77.4 | 37.8 | 293.4 | 3.43 ± 3.40 |
| `bun /Users/steven_chong/Downloads/repos/edgebox/bench/alloc_stress.js` | 23.6 ± 5.9 | 17.1 | 30.8 | 1.00 |
