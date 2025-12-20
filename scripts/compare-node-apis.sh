#!/bin/bash
# Compare Node.js API compatibility across EdgeBox, Bun, and Node.js
# Usage: ./scripts/compare-node-apis.sh <module>

set -e

MODULE="${1:-buffer}"
TIMESTAMP=$(date +%s)

echo "═══════════════════════════════════════════════════════════════"
echo "  Node.js API Compatibility Test - 3-Way Comparison"
echo "  Module: $MODULE"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Run tests on all 3 runtimes
echo "[1/3] Testing EdgeBox..."
START_EDGEBOX=$(date +%s)
./scripts/run-node-core-tests.sh . $MODULE > /dev/null 2>&1
END_EDGEBOX=$(date +%s)
EDGEBOX_TIME=$((END_EDGEBOX - START_EDGEBOX))

echo "[2/3] Testing Bun..."
START_BUN=$(date +%s)
./scripts/run-node-core-tests-native.sh bun $MODULE > /dev/null 2>&1
END_BUN=$(date +%s)
BUN_TIME=$((END_BUN - START_BUN))

echo "[3/3] Testing Node.js..."
START_NODE=$(date +%s)
./scripts/run-node-core-tests-native.sh node $MODULE > /dev/null 2>&1
END_NODE=$(date +%s)
NODE_TIME=$((END_NODE - START_NODE))

# Parse results
EDGEBOX_PASSED=$(grep "^passed:" node-test-results-${MODULE}.txt | cut -d: -f2 | tr -d ' ')
EDGEBOX_FAILED=$(grep "^failed:" node-test-results-${MODULE}.txt | cut -d: -f2 | tr -d ' ')
EDGEBOX_TOTAL=$((EDGEBOX_PASSED + EDGEBOX_FAILED))

BUN_PASSED=$(grep "^passed:" node-test-results-bun-${MODULE}.txt | cut -d: -f2 | tr -d ' ')
BUN_FAILED=$(grep "^failed:" node-test-results-bun-${MODULE}.txt | cut -d: -f2 | tr -d ' ')
BUN_TOTAL=$((BUN_PASSED + BUN_FAILED))

NODE_PASSED=$(grep "^passed:" node-test-results-node-${MODULE}.txt | cut -d: -f2 | tr -d ' ')
NODE_FAILED=$(grep "^failed:" node-test-results-node-${MODULE}.txt | cut -d: -f2 | tr -d ' ')
NODE_TOTAL=$((NODE_PASSED + NODE_FAILED))

# Calculate pass rates
if [ $EDGEBOX_TOTAL -gt 0 ]; then
  EDGEBOX_RATE=$(awk "BEGIN {printf \"%.1f%%\", ($EDGEBOX_PASSED / $EDGEBOX_TOTAL) * 100}")
else
  EDGEBOX_RATE="N/A"
fi

if [ $BUN_TOTAL -gt 0 ]; then
  BUN_RATE=$(awk "BEGIN {printf \"%.1f%%\", ($BUN_PASSED / $BUN_TOTAL) * 100}")
else
  BUN_RATE="N/A"
fi

if [ $NODE_TOTAL -gt 0 ]; then
  NODE_RATE=$(awk "BEGIN {printf \"%.1f%%\", ($NODE_PASSED / $NODE_TOTAL) * 100}")
else
  NODE_RATE="N/A"
fi

# Display comparison
echo ""
echo "═══════════════════════════════════════════════════════════════"
echo "                    COMPARISON RESULTS"
echo "═══════════════════════════════════════════════════════════════"
echo ""
printf "%-15s %-15s %-15s %-15s\n" "Runtime" "Pass/Total" "Pass Rate" "Time"
printf "%-15s %-15s %-15s %-15s\n" "-------" "----------" "---------" "----"
printf "%-15s %-15s %-15s %-15s\n" "EdgeBox" "$EDGEBOX_PASSED/$EDGEBOX_TOTAL" "$EDGEBOX_RATE" "${EDGEBOX_TIME}s"
printf "%-15s %-15s %-15s %-15s\n" "Bun" "$BUN_PASSED/$BUN_TOTAL" "$BUN_RATE" "${BUN_TIME}s"
printf "%-15s %-15s %-15s %-15s\n" "Node.js" "$NODE_PASSED/$NODE_TOTAL" "$NODE_RATE" "${NODE_TIME}s"
echo ""

# Find fastest
FASTEST="EdgeBox"
FASTEST_TIME=$EDGEBOX_TIME
if [ $BUN_TIME -lt $FASTEST_TIME ]; then
  FASTEST="Bun"
  FASTEST_TIME=$BUN_TIME
fi
if [ $NODE_TIME -lt $FASTEST_TIME ]; then
  FASTEST="Node.js"
  FASTEST_TIME=$NODE_TIME
fi

echo "⚡ Fastest: $FASTEST (${FASTEST_TIME}s)"
echo ""

# Save comparison summary
cat > node-test-comparison-${MODULE}.txt << EOF
Module: $MODULE
Timestamp: $(date)

EdgeBox: $EDGEBOX_PASSED/$EDGEBOX_TOTAL ($EDGEBOX_RATE) - ${EDGEBOX_TIME}s
Bun:     $BUN_PASSED/$BUN_TOTAL ($BUN_RATE) - ${BUN_TIME}s
Node.js: $NODE_PASSED/$NODE_TOTAL ($NODE_RATE) - ${NODE_TIME}s

Fastest: $FASTEST (${FASTEST_TIME}s)
EOF

echo "Comparison saved to: node-test-comparison-${MODULE}.txt"
