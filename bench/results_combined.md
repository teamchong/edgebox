# EdgeBox Benchmark Results

## startup

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `EdgeBox (Binary)` | 5.9 ± 0.6 | 5.0 | 6.7 | 1.00 |
| `EdgeBox (AOT)` | 63.3 ± 1.5 | 61.2 | 66.0 | 10.75 ± 1.07 |
| `EdgeBox (WASM)` | 59.2 ± 1.4 | 57.1 | 61.6 | 10.06 ± 1.00 |
| `Bun` | 14.7 ± 1.0 | 13.2 | 16.1 | 2.51 ± 0.30 |
| `Node.js` | 33.9 ± 1.2 | 31.9 | 35.7 | 5.76 ± 0.59 |

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
| EdgeBox (Binary) | 2936ms |
| EdgeBox (AOT) | 2853ms |
| EdgeBox (WASM) | 3769ms |
| Bun | 4314ms |
| Node.js | 7669ms |

## loop

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 45.6ms |
| EdgeBox (AOT) | 1574.0ms |
| EdgeBox (WASM) | 5560.9ms |
| Bun | 50.5ms |
| Node.js | 72.2ms |

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
| EdgeBox (Binary) | 47.4ms |
| EdgeBox (AOT) | 51061.2ms |
| EdgeBox (WASM) | 117436.0ms |
| Bun | 386.4ms |
| Node.js | 1054.1ms |

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
| EdgeBox (Binary) | 11175.8ms |
| EdgeBox (AOT) | 16805.2ms |
| EdgeBox (WASM) | 60828.6ms |
| Bun | 525.6ms |
| Node.js | 913.0ms |

## path_trace

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 0.3ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | 0.1ms |
| Bun | 10.3ms |
| Node.js | 14.4ms |

