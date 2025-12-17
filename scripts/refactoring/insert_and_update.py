#!/usr/bin/env python3
"""
Insert emitCommonOpcode and update both emitInstruction and emitTrampolineInstruction.
"""

import re
import os

# Get script directory and repo root
script_dir = os.path.dirname(os.path.abspath(__file__))
repo_root = os.path.dirname(os.path.dirname(script_dir))

# Read the generated emitCommonOpcode (use fixed version if available)
emit_common_path = '/tmp/emit_common_opcode_fixed.zig' if os.path.exists('/tmp/emit_common_opcode_fixed.zig') else '/tmp/emit_common_opcode.zig'
with open(emit_common_path, 'r') as f:
    common_opcode_func = f.read()

# Read the source file
with open(os.path.join(repo_root, 'src/freeze/codegen_ssa.zig'), 'r') as f:
    lines = f.readlines()

# Read shared opcodes
with open(os.path.join(script_dir, 'shared_ops.txt'), 'r') as f:
    shared_opcodes = set(line.strip() for line in f)

print(f"Inserting emitCommonOpcode and removing {len(shared_opcodes)} duplicates...")

# Find emitTrampolineInstruction
tramp_start = None
for i, line in enumerate(lines):
    if 'fn emitTrampolineInstruction(self: *SSACodeGen, instr: Instruction)' in line:
        tramp_start = i
        break

# Find the old emitCommonOpcode (if exists) to replace it
old_common_start = None
for i in range(tramp_start - 1, 0, -1):
    if '/// Emit opcodes that are shared between emitTrampolineInstruction and emitInstruction' in lines[i]:
        old_common_start = i
        break

print(f"emitTrampolineInstruction at line {tramp_start}")
if old_common_start:
    print(f"Old emitCommonOpcode at line {old_common_start} (will replace)")

# Build output
output = []
i = 0

while i < len(lines):
    line = lines[i]

    # Replace old emitCommonOpcode if it exists
    if old_common_start and i == old_common_start:
        # Insert new emitCommonOpcode
        output.append(common_opcode_func)
        output.append('\n')

        # Skip old emitCommonOpcode until emitTrampolineInstruction
        while i < len(lines) and 'fn emitTrampolineInstruction' not in lines[i]:
            i += 1
        # Don't increment i, let it continue to handle emitTrampolineInstruction
        continue

    # Insert emitCommonOpcode before emitTrampolineInstruction if there was no old one
    if not old_common_start and i == tramp_start:
        output.append(common_opcode_func)
        output.append('\n')

    # Update emitTrampolineInstruction
    if 'fn emitTrampolineInstruction(self: *SSACodeGen, instr: Instruction)' in line:
        output.append(line)  # fn line
        i += 1

        # Copy lines until switch statement
        while i < len(lines) and 'switch (instr.opcode)' not in lines[i]:
            output.append(lines[i])
            i += 1

        # Copy switch line
        output.append(lines[i])  # switch line
        i += 1

        # Add call to emitCommonOpcode
        output.append("            // Try shared opcodes first\n")
        output.append("            if (try self.emitCommonOpcode(instr)) return;\n")
        output.append("\n")

        # Skip shared opcodes
        while i < len(lines):
            curr_line = lines[i]

            # Stop at next function
            if re.match(r'^\s*fn\s+', curr_line):
                break

            # Skip old call to emitCommonOpcode if it exists
            if 'if (try self.emitCommonOpcode(instr)) return;' in curr_line:
                i += 1
                continue

            # Check if this is a shared opcode
            match = re.match(r'\s*\.(\w+)(\s*,\s*\.(\w+))?\s*=>', curr_line)
            if match:
                op1 = match.group(1)
                op2 = match.group(3)

                if op1 in shared_opcodes or (op2 and op2 in shared_opcodes):
                    # Skip this opcode
                    i += 1
                    brace_count = curr_line.count('{') - curr_line.count('}')

                    # Skip until end of case
                    while i < len(lines):
                        curr_line = lines[i]

                        # Stop at next case or else
                        if re.match(r'\s*\.', curr_line) and '=>' in curr_line:
                            break
                        if curr_line.strip().startswith('else =>'):
                            break

                        brace_count += curr_line.count('{') - curr_line.count('}')
                        i += 1

                        if brace_count == 0 and curr_line.strip() in ['},', '},\n']:
                            break

                    continue

            output.append(curr_line)
            i += 1
        continue

    # Update emitInstruction
    if 'fn emitInstruction(self: *SSACodeGen, instr: Instruction)' in line:
        output.append(line)  # fn line
        i += 1

        # Copy lines until switch statement
        while i < len(lines) and 'switch (instr.opcode)' not in lines[i]:
            output.append(lines[i])
            i += 1

        # Copy switch line
        output.append(lines[i])  # switch line
        i += 1

        # Add call to emitCommonOpcode
        output.append("            // Try shared opcodes first\n")
        output.append("            if (try self.emitCommonOpcode(instr)) return;\n")
        output.append("\n")

        # Skip shared opcodes
        while i < len(lines):
            curr_line = lines[i]

            # Stop at next function or end of file
            if re.match(r'^\s*fn\s+', curr_line):
                break

            # Skip old call to emitCommonOpcode if it exists
            if 'if (try self.emitCommonOpcode(instr)) return;' in curr_line:
                i += 1
                continue

            # Check if this is a shared opcode
            match = re.match(r'\s*\.(\w+)(\s*,\s*\.(\w+))?\s*=>', curr_line)
            if match:
                op1 = match.group(1)
                op2 = match.group(3)

                if op1 in shared_opcodes or (op2 and op2 in shared_opcodes):
                    # Skip this opcode
                    i += 1
                    brace_count = curr_line.count('{') - curr_line.count('}')

                    # Skip until end of case
                    while i < len(lines):
                        curr_line = lines[i]

                        # Stop at next case or else
                        if re.match(r'\s*\.', curr_line) and '=>' in curr_line:
                            break
                        if curr_line.strip().startswith('else =>'):
                            break

                        brace_count += curr_line.count('{') - curr_line.count('}')
                        i += 1

                        if brace_count == 0 and curr_line.strip() in ['},', '},\n']:
                            break

                    continue

            output.append(curr_line)
            i += 1
        continue

    output.append(line)
    i += 1

# Write output
with open('/tmp/codegen_ssa_final.zig', 'w') as f:
    f.writelines(output)

print(f"Original: {len(lines)} lines")
print(f"Refactored: {len(output)} lines")
print(f"Removed: {len(lines) - len(output)} lines")
print("Wrote to /tmp/codegen_ssa_final.zig")
