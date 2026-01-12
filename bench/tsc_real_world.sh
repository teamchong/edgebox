#!/bin/bash
# Real-World TSC Benchmark
# Compares TypeScript compilation speed between Node.js, Bun, and EdgeBox

set -e
BENCH_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$BENCH_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}╔════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║           Real-World TSC Benchmark                         ║${NC}"
echo -e "${BLUE}║   Comparing: Node.js vs Bun vs EdgeBox                     ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check dependencies
echo -e "${YELLOW}Checking dependencies...${NC}"
NODE_VERSION=$(node --version 2>/dev/null || echo "not found")
BUN_VERSION=$(bun --version 2>/dev/null || echo "not found")

echo "  Node.js: $NODE_VERSION"
echo "  Bun: $BUN_VERSION"
echo ""

# Benchmark function using hyperfine if available, else manual timing
benchmark() {
  local name="$1"
  local cmd="$2"
  local warmup="${3:-2}"
  local runs="${4:-5}"

  echo -e "${YELLOW}$name${NC}"

  if command -v hyperfine &> /dev/null; then
    hyperfine --warmup "$warmup" --runs "$runs" --shell=none "$cmd" 2>&1 | grep -E "Time|mean"
  else
    local times=()
    # Warmup
    for i in $(seq 1 $warmup); do
      eval "$cmd" > /dev/null 2>&1
    done
    # Timed runs
    for i in $(seq 1 $runs); do
      local start=$(date +%s%3N)
      eval "$cmd" > /dev/null 2>&1
      local end=$(date +%s%3N)
      local elapsed=$((end - start))
      times+=($elapsed)
    done
    # Calculate average
    local sum=0
    for t in "${times[@]}"; do
      sum=$((sum + t))
    done
    local avg=$((sum / runs))
    echo -e "  ${GREEN}Average: ${avg}ms${NC} (runs: ${times[*]})"
  fi
  echo ""
}

echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Test 1: TypeScript transpileModule (tsc_bench.js)${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""

benchmark "Node.js" "node typescript/tsc_bench.js"
benchmark "Bun" "bun typescript/tsc_bench.js"

# Check for EdgeBox AOT
EDGEBOX="../zig-out/bin/edgebox"
AOT_PATH="../zig-out/bin/bench/tsc_bench.aot"

if [ -f "$EDGEBOX" ] && [ -f "$AOT_PATH" ]; then
  benchmark "EdgeBox AOT" "$EDGEBOX $AOT_PATH"
else
  echo -e "${RED}EdgeBox AOT not found. Build with: edgeboxc build bench/typescript/tsc_bench.js${NC}"
  echo ""
fi

echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Test 2: TSC startup (--version)${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""

benchmark "Node.js tsc --version" "node typescript/bin/tsc --version" 3 10
benchmark "Bun tsc --version" "bun typescript/bin/tsc --version" 3 10

echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Summary${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""
echo "Lower times are better."
echo ""
echo "Note: To test EdgeBox, first build the TSC benchmark:"
echo "  cd $(dirname $BENCH_DIR)"
echo "  zig-out/bin/edgeboxc build bench/typescript/tsc_bench.js"
echo ""
