| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `/Users/steven_chong/Downloads/repos/edgebox/zig-out/bin/edgebox /Users/steven_chong/Downloads/repos/edgebox/bench/hello.wasm` | 16.5 ± 0.7 | 15.4 | 17.3 | 1.16 ± 0.13 |
| `bun /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 14.3 ± 1.5 | 11.6 | 15.5 | 1.00 |
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/.wasmedge/lib/wasmedge_quickjs_aot.wasm /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 17.9 ± 1.0 | 16.9 | 19.5 | 1.25 ± 0.15 |
| `node /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 30.8 ± 0.6 | 30.2 | 31.6 | 2.15 ± 0.23 |
| `porffor /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 186.5 ± 3.0 | 184.0 | 190.6 | 13.04 ± 1.42 |
