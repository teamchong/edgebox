# EdgeBox Benchmark Results

## startup

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `EdgeBox (Binary)` | 4.2 ± 0.5 | 3.3 | 5.1 | 1.00 |
| `Bun` | 12.5 ± 1.1 | 10.9 | 14.2 | 2.96 ± 0.43 |
| `Node.js` | 33.7 ± 1.0 | 32.2 | 35.1 | 7.95 ± 0.94 |

## memory

| Runtime | Memory |
|:---|---:|
| EdgeBox (Binary) | 112.2MB |
| Bun | 104.6MB |
| Node.js | 143.1MB |

## fib

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 3011ms |
| Bun | 4690ms |
| Node.js | 8953ms |

## loop

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 77.3ms |
| Bun | 50.8ms |
| Node.js | 74.5ms |

## tail_recursive

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 18.0ms |
| Bun | 43953.9ms |
| Node.js | 90871.9ms |

## typed_array

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 49.0ms |
| Bun | 390.5ms |
| Node.js | 1126.4ms |

## mandelbrot

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 251.8ms |
| Bun | 10.8ms |
| Node.js | 12.2ms |

## prime_factors

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 98.8ms |
| Bun | 7.4ms |
| Node.js | 11.9ms |

## gaussian_blur

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 0.6ms |
| Bun | 2.8ms |
| Node.js | 5.1ms |

## average

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 768.9ms |
| Bun | 516.4ms |
| Node.js | 930.9ms |

## path_trace

| Runtime | Time |
|:---|---:|
| EdgeBox (Binary) | 186.7ms |
| Bun | 9.6ms |
| Node.js | 15.3ms |

