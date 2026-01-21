# EdgeBox Benchmark Results

## startup

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `EdgeBox (Binary)` | 4.6 ± 0.4 | 4.1 | 5.4 | 1.00 |
| `EdgeBox (AOT)` | 80.1 ± 6.3 | 71.9 | 92.8 | 17.29 ± 2.09 |
| `EdgeBox (WASM)` | 58.8 ± 0.9 | 57.6 | 60.6 | 12.70 ± 1.18 |
| `Bun` | 14.0 ± 1.0 | 11.8 | 15.3 | 3.03 ± 0.35 |
| `Node.js` | 33.5 ± 1.3 | 31.2 | 35.1 | 7.24 ± 0.72 |

## memory

| Runtime | Memory |
|:---|---:|
| EdgeBox (Binary) | 112.1MB |
| EdgeBox (AOT) | 1.5MB |
| EdgeBox (WASM) | 219.0MB |
| Bun | 113.6MB |
| Node.js | 145.2MB |

## fib

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | FAIL |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | FAIL |
| Node.js | FAIL |

## loop

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 44.2ms |
| EdgeBox (AOT) | 1260.5ms |
| EdgeBox (WASM) | 4185.0ms |
| Bun | 49.5ms |
| Node.js | 71.6ms |

## tail_recursive

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 87.7ms |
| EdgeBox (AOT) | 181.2ms |
| EdgeBox (WASM) | 406.8ms |
| Bun | 41760.4ms |
| Node.js | 86971.8ms |

## typed_array

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 46.0ms |
| EdgeBox (AOT) | 46259.7ms |
| EdgeBox (WASM) | 100511.9ms |
| Bun | 397.9ms |
| Node.js | 1077.8ms |

## mandelbrot

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 72.6ms |
| EdgeBox (AOT) | 78.4ms |
| EdgeBox (WASM) | 281.6ms |
| Bun | 11.8ms |
| Node.js | 11.3ms |

## prime_factors

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 42.9ms |
| EdgeBox (AOT) | 43.1ms |
| EdgeBox (WASM) | 59.6ms |
| Bun | 8.4ms |
| Node.js | 12.2ms |

## gaussian_blur

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 4.7ms |
| EdgeBox (AOT) | 101.8ms |
| EdgeBox (WASM) | 171.6ms |
| Bun | 2.6ms |
| Node.js | 5.4ms |

## average

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 11299.6ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | 534.2ms |
| Node.js | 940.6ms |

## path_trace

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 0.3ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | 0.1ms |
| Bun | 10.3ms |
| Node.js | 14.4ms |

