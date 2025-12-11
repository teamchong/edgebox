# EdgeBox Benchmarks

Benchmark suite comparing EdgeBox performance against other JS runtimes.

## Quick Run

```bash
./bench/run_hyperfine.sh
```

## Benchmark Results

Tested on macOS (Apple Silicon M3 Max), December 2024.

### 1. Cold Start (hello world)

| Runtime | Time | vs Fastest |
|---------|------|------------|
| Porffor (CLI) | 7.0ms | 1.0x |
| EdgeBox (daemon) | 17.1ms | 2.4x |
| Bun (CLI) | 17.8ms | 2.5x |
| Node.js (CLI) | 35.2ms | 5.0x |
| Porffor (WASM) | 99.0ms | 14.2x |
| EdgeBox (WASM) | 150.8ms | 21.6x |

### 2. Allocator Stress (30k allocations)

| Runtime | Time | vs Fastest |
|---------|------|------------|
| Bun (CLI) | 20.5ms | 1.0x |
| EdgeBox (daemon) | 28.8ms | 1.4x |
| Node.js (CLI) | 38.5ms | 1.9x |
| Porffor (CLI) | 48.0ms | 2.3x |
| EdgeBox (WASM) | 161.8ms | 7.9x |
| Porffor (WASM) | 280.6ms | 13.7x |

### 3. CPU Benchmark - fib(35)

| Runtime | Time | vs Fastest |
|---------|------|------------|
| Bun (CLI) | 62.6ms | 1.0x |
| Node.js (CLI) | 99.4ms | 1.6x |
| Porffor (CLI) | 139.1ms | 2.2x |
| Porffor (WASM) | 198.7ms | 3.2x |
| EdgeBox (daemon) | 996.4ms | 15.9x |
| EdgeBox (WASM) | 1137ms | 18.2x |

### 4. Daemon Warm Pod (pre-allocated pool)

| Runtime | Time |
|---------|------|
| EdgeBox (daemon warm) | 9.4ms |

With 32 pre-allocated instances, daemon response time drops to <10ms.

## Frozen Interpreter (edgebox-freeze)

For CPU-bound workloads, the frozen interpreter transpiles QuickJS bytecode to optimized C:

| Implementation | fib(35) | Speedup |
|---------------|---------|---------|
| EdgeBox Frozen (SSA) | ~51ms | **18x faster** |
| EdgeBox Interpreted | ~919ms | baseline |
| Bun (JIT) | ~63ms | reference |

The frozen interpreter achieves near-JIT performance by:
- Native int32 arithmetic (no JSValue boxing)
- Direct recursive calls (no runtime dispatch)
- LLVM optimization of generated C code

```bash
# Build and use frozen interpreter
zig build freeze -Doptimize=ReleaseFast
./zig-out/bin/edgebox-freeze bytecode.c -o frozen.c -n my_func
```

## Analysis

**Cold Start**: EdgeBox WASM has ~150ms cold start due to AOT compilation overhead. The daemon mode eliminates this by keeping instances warm (17ms). Porffor native is fastest at 7ms.

**Memory Allocation**: Bun's JSC allocator is fastest. EdgeBox daemon is competitive at 1.4x. WASM modes have overhead from linear memory management.

**CPU (Interpreted)**: QuickJS interpreter is ~16-18x slower than JIT runtimes (Bun/Node). This is expected - QuickJS is a bytecode interpreter, not a JIT compiler.

**CPU (Frozen)**: The frozen interpreter closes the gap to ~1.2x of Bun by transpiling bytecode to native C code, bypassing interpreter overhead entirely.

## When to Use EdgeBox

✅ **Good fit:**
- Serverless/edge functions (daemon mode)
- Security-sensitive workloads (WASM sandbox)
- Embedding JS in native apps
- CPU-bound functions (with frozen interpreter)

⚠️ **Consider alternatives:**
- CPU-intensive interpreted code (use frozen interpreter or Bun/Node)
- Latency-critical cold starts (use daemon mode or Bun)

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
