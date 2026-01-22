# EdgeBox Benchmark Results

## startup

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `EdgeBox (Binary)` | 9.6 ± 7.7 | 4.6 | 28.8 | 1.00 |
| `EdgeBox (AOT)` | 145.9 ± 97.7 | 74.6 | 329.8 | 15.22 ± 15.93 |
| `EdgeBox (WASM)` | 66.4 ± 8.3 | 59.2 | 79.3 | 6.93 ± 5.64 |
| `Bun` | 16.0 ± 2.2 | 14.2 | 20.6 | 1.67 ± 1.36 |
| `Node.js` | 40.1 ± 4.6 | 34.7 | 47.2 | 4.19 ± 3.40 |

## memory

| Runtime | Memory |
|:---|---:|
| EdgeBox (Binary) | 112.1MB |
| EdgeBox (AOT) | 1.5MB |
| EdgeBox (WASM) | 131.6MB |
| Bun | 104.5MB |
| Node.js | 145.6MB |

## fib

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 3061ms |
| EdgeBox (AOT) | 2923ms |
| EdgeBox (WASM) | 3962ms |
| Bun | 4509ms |
| Node.js | 7981ms |

## loop

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 51.9ms |
| EdgeBox (AOT) | 1738.8ms |
| EdgeBox (WASM) | 6360.7ms |
| Bun | 69.4ms |
| Node.js | 77.6ms |

## tail_recursive

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 95.2ms |
| EdgeBox (AOT) | 213.6ms |
| EdgeBox (WASM) | 448.0ms |
| Bun | 53732.3ms |
| Node.js | 91184.6ms |

## typed_array

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 48.7ms |
| EdgeBox (AOT) | 56948.9ms |
| EdgeBox (WASM) | 130666.1ms |
| Bun | 419.1ms |
| Node.js | 1094.1ms |

## mandelbrot

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 76.9ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | 349.9ms |
| Bun | 12.8ms |
| Node.js | 14.0ms |

## prime_factors

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 32.7ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | 592.2ms |
| Bun | 10.1ms |
| Node.js | 13.9ms |

## gaussian_blur

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 7.3ms |
| EdgeBox (AOT) | FAIL |
| EdgeBox (WASM) | 206.4ms |
| Bun | 6.3ms |
| Node.js | 5.7ms |

## average

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 647.2ms |
| EdgeBox (AOT) | 25069.5ms |
| EdgeBox (WASM) | 62359.1ms |
| Bun | 569.6ms |
| Node.js | 943.7ms |

## path_trace

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 0.1ms |
| EdgeBox (AOT) | 0.0ms |
| EdgeBox (WASM) | 0.0ms |
| Bun | 12.0ms |
| Node.js | 16.1ms |

