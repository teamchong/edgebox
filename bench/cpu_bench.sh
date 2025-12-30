#!/bin/bash
# CPU Benchmark Runner
# Runs CPU-bound benchmarks comparing EdgeBox vs Bun

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}                    CPU Benchmark Suite${NC}"
echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}"
echo ""

# Build EdgeBox if needed
echo -e "${YELLOW}Building EdgeBox...${NC}"
cd "$REPO_ROOT"
zig build -Doptimize=ReleaseFast -Dcpu=baseline 2>/dev/null || zig build

EDGEBOX="$REPO_ROOT/zig-out/bin/edgebox"

if [ ! -f "$EDGEBOX" ]; then
    echo "Error: EdgeBox binary not found at $EDGEBOX"
    exit 1
fi

# Check for Bun
if ! command -v bun &> /dev/null; then
    echo "Warning: Bun not found, running EdgeBox only"
    BUN_AVAILABLE=false
else
    BUN_AVAILABLE=true
fi

# Results file
RESULTS_FILE="$SCRIPT_DIR/results_cpu.md"

# Run a single benchmark
run_benchmark() {
    local name="$1"
    local file="$2"

    echo ""
    echo -e "${GREEN}=== $name ===${NC}"

    # Run with Bun first (baseline)
    if [ "$BUN_AVAILABLE" = true ]; then
        echo -e "${YELLOW}Running with Bun...${NC}"
        BUN_OUTPUT=$(bun "$SCRIPT_DIR/cpu/$file" 2>&1)
        BUN_JSON=$(echo "$BUN_OUTPUT" | grep "^JSON_RESULTS:" | sed 's/^JSON_RESULTS://')
        echo "$BUN_OUTPUT" | grep -v "^JSON_RESULTS:"
    fi

    # Run with EdgeBox
    echo ""
    echo -e "${YELLOW}Running with EdgeBox...${NC}"
    EDGEBOX_OUTPUT=$("$EDGEBOX" "$SCRIPT_DIR/cpu/$file" 2>&1) || true
    EDGEBOX_JSON=$(echo "$EDGEBOX_OUTPUT" | grep "^JSON_RESULTS:" | sed 's/^JSON_RESULTS://')
    echo "$EDGEBOX_OUTPUT" | grep -v "^JSON_RESULTS:"

    # Store results for markdown
    echo "### $name" >> "$RESULTS_FILE"
    echo "" >> "$RESULTS_FILE"

    if [ "$BUN_AVAILABLE" = true ] && [ -n "$BUN_JSON" ]; then
        echo "**Bun Results:**" >> "$RESULTS_FILE"
        echo '```' >> "$RESULTS_FILE"
        echo "$BUN_OUTPUT" | grep -v "^JSON_RESULTS:" | grep -v "^===" >> "$RESULTS_FILE"
        echo '```' >> "$RESULTS_FILE"
        echo "" >> "$RESULTS_FILE"
    fi

    if [ -n "$EDGEBOX_JSON" ]; then
        echo "**EdgeBox Results:**" >> "$RESULTS_FILE"
        echo '```' >> "$RESULTS_FILE"
        echo "$EDGEBOX_OUTPUT" | grep -v "^JSON_RESULTS:" | grep -v "^===" >> "$RESULTS_FILE"
        echo '```' >> "$RESULTS_FILE"
    else
        echo "_EdgeBox results pending_" >> "$RESULTS_FILE"
    fi
    echo "" >> "$RESULTS_FILE"
}

# Initialize results file
echo "# CPU Benchmark Results" > "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"
echo "_Generated: $(date -u '+%Y-%m-%d %H:%M:%S UTC')_" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"
echo "## Benchmarks" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Run all CPU benchmarks
run_benchmark "JSON Pipeline" "json_pipeline.js"
run_benchmark "Base64 Encoding" "base64.js"
run_benchmark "Buffer Operations" "buffer_ops.js"
run_benchmark "UTF-8 Codec" "utf8_codec.js"

echo ""
echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}                    Benchmark Complete${NC}"
echo -e "${GREEN}═══════════════════════════════════════════════════════════════${NC}"
echo ""
echo "Results saved to: $RESULTS_FILE"
