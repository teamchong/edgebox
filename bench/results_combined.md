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
| EdgeBox (Binary) | 3043.00ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | 4399.37ms |
| Node.js | 7661.07ms |

## loop

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 46.8ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | 380.7ms |
| Node.js | 1047.1ms |

## tail_recursive

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 90.3ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | 40729.0ms |
| Node.js | 83922.6ms |

## typed_array

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 46.7ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | 380.3ms |
| Node.js | 1036.2ms |

## mandelbrot

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 69.2ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | 24.6ms |
| Node.js | 11.4ms |

## prime_factors

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 95.3ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | 21.3ms |
| Node.js | 11.0ms |

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

