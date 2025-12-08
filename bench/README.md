# EdgeBox Benchmarks

Benchmark suite comparing EdgeBox performance against other JS runtimes.

## Quick Run

```bash
./bench/run_hyperfine.sh
```

## Benchmark Results

Tested on macOS (Apple Silicon), December 2024.

### Cold Start (hello world)

| Runtime | P50 | P90 | P99 |
|---------|-----|-----|-----|
| EdgeBox (AOT + bytecode cache) | ~60ms | ~65ms | ~70ms |
| Node.js | ~38ms | ~40ms | ~45ms |
| Bun | ~21ms | ~23ms | ~25ms |

### Allocator Stress (30k allocations)

| Runtime | Time | Throughput |
|---------|------|------------|
| EdgeBox (pool allocator) | 24ms | 1.25M allocs/sec |
| Node.js (V8) | 4ms | 7.5M allocs/sec |
| Bun (JSC) | 6ms | 5M allocs/sec |

## Pool Allocator

EdgeBox uses a custom WASM pool allocator by default:

- **O(1) malloc** - freelist pop or bump allocation
- **O(1) free** - push to size-class freelist
- **O(1) reset** - bulk cleanup at request end

The pool allocator is enabled by default for WASM builds. Use `--no-pool-allocator` to disable for debugging.

## Benchmarks

| File | Description |
|------|-------------|
| `hello.js` | Cold start measurement |
| `fib.js` | CPU benchmark (recursive fibonacci) |
| `alloc_stress.js` | Allocator performance (30k allocations) |

## What is P50/P90/P99?

Percentile latencies measure response time distribution:
- **P50** (median): 50% of requests are faster
- **P90**: 90% of requests are faster (tail starts here)
- **P99**: 99% of requests are faster (worst 1%)

Averages hide outliers. P99 shows real worst-case user experience.
