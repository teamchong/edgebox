# EdgeBox Benchmark Results

## startup

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `EdgeBox (Binary)` | 9.2 ± 5.1 | 2.7 | 19.1 | 1.00 |
| `EdgeBox (AOT)` | 375.8 ± 179.7 | 128.2 | 707.8 | 40.95 ± 30.19 |
| `EdgeBox (WASM)` | 95.7 ± 50.3 | 66.6 | 234.7 | 10.42 ± 8.01 |
| `Bun` | 20.4 ± 5.0 | 9.2 | 27.3 | 2.23 ± 1.36 |
| `Node.js` | 40.5 ± 10.3 | 30.4 | 59.4 | 4.41 ± 2.72 |

## memory

| Runtime | Memory |
|:---|---:|
| EdgeBox (Binary) | 112.1MB |
| EdgeBox (AOT) | 1.5MB |
| EdgeBox (WASM) | 96.3MB |
| Bun | 104.5MB |
| Node.js | 145.2MB |

## fib

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 3328.35ms |
| EdgeBox (AOT) | 3393.83ms |
| EdgeBox (WASM) | 3950.34ms |
| Bun | 4527.18ms |
| Node.js | 7935.81ms |

## loop

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 46.7ms |
| EdgeBox (AOT) | 1539.0ms |
| EdgeBox (WASM) | 5373.7ms |
| Bun | 59.6ms |
| Node.js | 72.3ms |

## tail_recursive

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 106.0ms |
| EdgeBox (AOT) | 212.7ms |
| EdgeBox (WASM) | 449.3ms |
| Bun | 43301.7ms |
| Node.js | 91690.5ms |

## typed_array

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 47.8ms |
| EdgeBox (AOT) | 47680.2ms |
| EdgeBox (WASM) | 113503.9ms |
| Bun | 391.9ms |
| Node.js | 1099.3ms |

## mandelbrot

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 387.8ms |
| EdgeBox (AOT) | 549.8ms |
| EdgeBox (WASM) | 955.0ms |
| Bun | 11.1ms |
| Node.js | 11.6ms |

## prime_factors

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 75.5ms |
| EdgeBox (AOT) | 95.5ms |
| EdgeBox (WASM) | 141.2ms |
| Bun | 8.2ms |
| Node.js | 11.4ms |

## gaussian_blur

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 4.6ms |
| EdgeBox (AOT) | 97.8ms |
| EdgeBox (WASM) | 170.0ms |
| Bun | 2.6ms |
| Node.js | 5.0ms |

## average

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 12828.8ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | FAIL |
| Bun | 505.0ms |
| Node.js | 911.7ms |

## path_trace

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 0.2ms |
| EdgeBox (AOT) | 0.1ms |
| EdgeBox (WASM) | 0.2ms |
| Bun | 10.6ms |
| Node.js | 14.8ms |

