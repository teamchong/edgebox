# TSC Benchmark Results

Benchmark comparing TypeScript compilation speed between Node.js and Bun.
Tests use real-world TypeScript patterns from popular libraries.

## Test Machine
- Apple M1/M2/M3 Mac (ARM64)
- Node.js v20.18.0
- Bun 1.3.5
- TypeScript 5.9.3

## Results

### Popular TypeScript Patterns

| Test | Node.js | Bun | Winner |
|------|---------|-----|--------|
| Zod-like Schema | 574ms | 523ms | Bun (+10%) |
| tRPC-like Router | 562ms | 516ms | Bun (+8%) |
| React-like Types | 563ms | 501ms | Bun (+11%) |
| Prisma-like ORM | 568ms | 481ms | Bun (+15%) |

### transpileModule (tsc_bench.js)

| Runtime | Time |
|---------|------|
| Node.js | 186ms |
| Bun | 179ms |

### TSC Startup (--version)

| Runtime | Time |
|---------|------|
| Node.js | 92ms |
| Bun | 88ms |

## Test Descriptions

1. **Zod-like Schema** - Schema validation with complex type inference (union types, optional fields, object schemas)
2. **tRPC-like Router** - Type-safe API router with procedure definitions and nested routers
3. **React-like Types** - React component patterns (FC, hooks, generics, props inference)
4. **Prisma-like ORM** - Database ORM types with model delegates, where clauses, and relations

## Running Benchmarks

```bash
# Full benchmark
./bench/tsc_real_world.sh

# CI benchmark (popular TS patterns)
./bench/tsc_ci_bench.sh

# Quick CI benchmark
./bench/tsc_ci_bench.sh --quick

# JSON output for CI
./bench/tsc_ci_bench.sh --json
```

## Notes

- Bun is consistently ~10-15% faster than Node.js for TypeScript compilation
- The difference is more pronounced with complex type patterns (Prisma-like ORM)
- EdgeBox AOT compilation requires ~4GB RAM for large projects (TSC is 60MB+)
