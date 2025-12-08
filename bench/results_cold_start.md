| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `/Users/steven_chong/Downloads/repos/edgebox/run.sh /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 73.3 ± 7.7 | 63.6 | 93.2 | 5.27 ± 0.66 |
| `wasmedge --dir /Users/steven_chong/Downloads/repos/edgebox/bench /Users/steven_chong/.wasmedge/lib/wasmedge_quickjs_aot.wasm /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 17.6 ± 0.8 | 16.5 | 19.4 | 1.26 ± 0.11 |
| `/Users/steven_chong/.local/share/mise/installs/node/20.18.0/lib/node_modules/porffor/porf /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 98.7 ± 1.9 | 94.8 | 103.2 | 7.10 ± 0.52 |
| `node /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 30.6 ± 0.6 | 29.3 | 31.7 | 2.20 ± 0.16 |
| `bun /Users/steven_chong/Downloads/repos/edgebox/bench/hello.js` | 13.9 ± 1.0 | 12.3 | 16.3 | 1.00 |
