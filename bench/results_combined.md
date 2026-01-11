# EdgeBox Benchmark Results

## startup

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `EdgeBox (AOT)` | 476.2 ± 175.6 | 10.5 | 664.4 | 19.91 ± 17.57 |
| `EdgeBox (WASM)` | 429.6 ± 83.2 | 296.1 | 540.1 | 17.96 ± 14.81 |
| `Bun` | 23.9 ± 19.2 | 7.7 | 70.2 | 1.00 |
| `Node.js` | 40.3 ± 10.5 | 33.9 | 69.3 | 1.69 ± 1.42 |

## memory

| Runtime | Memory |
|:---|---:|
| EdgeBox (AOT) | 1.3MB |
| EdgeBox (WASM) | 1.3MB |
| Bun | 104.0MB |
| Node.js | 141.4MB |

## fib

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 2979.35ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | 5476.94ms |
| Node.js | 7811.70ms |

## loop

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 57.7ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | 764.3ms |
| Node.js | 1352.8ms |

## tail_recursive

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 89.9ms |
| EdgeBox (AOT) | 320.9ms |
| EdgeBox (WASM) | 5199.3ms |
| Bun | 5140.1ms |
| Node.js | 122531.8ms |

## typed_array

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 66.8ms |
| EdgeBox (AOT) | 171.4ms |
| EdgeBox (WASM) | 5769.8ms |
| Bun | 500.3ms |
| Node.js | 1292.4ms |

## mandelbrot

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 89.9ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | 18.1ms |
| Node.js | 17.6ms |

## prime_factors

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | FAIL |
| EdgeBox (AOT) | 405.5ms |
| EdgeBox (WASM) | 13095.1ms |
| Bun | 41.2ms |
| Node.js | 18.8ms |

## gaussian_blur

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 0.1ms |
| EdgeBox (AOT) | TIMEOUT |
| EdgeBox (WASM) | TIMEOUT |
| Bun | 21.4ms |
| Node.js | 8.7ms |

## average

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | FAIL |
| EdgeBox (AOT) | 5433.6ms |
| EdgeBox (WASM) | 207787.6ms |
| Bun | 689.7ms |
| Node.js | 1267.1ms |

## path_trace

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 245.1ms |
| EdgeBox (AOT) | 272.9ms |
| EdgeBox (WASM) | 7152.5ms |
| Bun | 24.5ms |
| Node.js | 39.1ms |

