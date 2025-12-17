#!/usr/bin/env python3
"""
Extract shared opcodes - robust version with proper brace matching.
"""

import re
import os

# Get script directory and repo root
script_dir = os.path.dirname(os.path.abspath(__file__))
repo_root = os.path.dirname(os.path.dirname(script_dir))

# Read source
with open(os.path.join(repo_root, 'src/freeze/codegen_ssa.zig'), 'r') as f:
    content = f.read()

# Read shared opcodes
with open(os.path.join(script_dir, 'shared_ops.txt'), 'r') as f:
    shared_opcodes = set(line.strip() for line in f)

print(f"Extracting {len(shared_opcodes)} shared opcodes...")

# Split into lines for processing
lines = content.split('\n')

# Find emitTrampolineInstruction (simpler implementations)
emit_start = None
for i, line in enumerate(lines):
    if 'fn emitTrampolineInstruction(self: *SSACodeGen, instr: Instruction)' in line:
        emit_start = i
        break

# Find switch statement start
switch_start = None
for i in range(emit_start, len(lines)):
    if 'switch (instr.opcode)' in lines[i]:
        switch_start = i + 1  # Start after the switch line
        break

print(f"emitTrampolineInstruction at line {emit_start}, switch at {switch_start}")

# Extract opcodes
extracted = {}
i = switch_start

while i < len(lines):
    line = lines[i]

    # Stop at end of function or else => unreachable
    if re.match(r'^\s*fn\s+', line) or line.strip() == 'else => unreachable,':
        break

    # Look for opcode case
    match = re.match(r'(\s*)\.(\w+)(\s*,\s*\.(\w+))?\s*=>', line)
    if match:
        indent = match.group(1)
        op1 = match.group(2)
        op2 = match.group(4)

        # Check if shared
        is_shared = op1 in shared_opcodes or (op2 and op2 in shared_opcodes)

        if is_shared:
            # Capture full case implementation
            case_text = [line]
            i += 1

            # Check if it's a one-liner: .op => expr,
            if '=>' in line and line.rstrip().endswith(',') and '{' not in line:
                # One-liner, already captured
                extracted[op1] = case_text[0]  # Just the line itself
                continue

            # Multi-line case - count braces properly
            brace_count = line.count('{') - line.count('}')

            while i < len(lines):
                curr = lines[i]

                # Stop at next case
                if re.match(r'\s*\.', curr) and '=>' in curr:
                    break

                # Stop at else clause
                if curr.strip().startswith('else =>'):
                    break

                case_text.append(curr)
                brace_count += curr.count('{') - curr.count('}')
                i += 1

                # Case is complete when braces balance and we see },
                if brace_count == 0 and curr.strip() == '},':
                    break

            extracted[op1] = '\n'.join(case_text)  # Use newline to join!
            continue

    i += 1

print(f"Extracted {len(extracted)} shared opcodes")

# Build emitCommonOpcode
output = []
output.append("    /// Emit opcodes that are shared between emitTrampolineInstruction and emitInstruction\n")
output.append("    /// Returns true if the opcode was handled, false if caller should handle it\n")
output.append("    fn emitCommonOpcode(self: *SSACodeGen, instr: Instruction) !bool {\n")
output.append("        const debug = self.options.debug_comments;\n")
output.append("\n")
output.append("        switch (instr.opcode) {\n")

# Add extracted opcodes with return true
for opcode in sorted(extracted.keys()):
    impl = extracted[opcode]

    # Check if this is a simple expression (even if multi-line)
    # Simple expression: .op => try self.xxx(...),
    if impl.strip().endswith('),') and '\n' in impl:
        # Multi-line expression ending with ), - needs to become a block
        lines_in_impl = impl.split('\n')

        # First line has .opcode => - add opening brace
        first_line = lines_in_impl[0]
        if '=>' in first_line:
            parts = first_line.split('=>', 1)
            output.append(f"{parts[0]}=> {{\n")
            # If there's code after => on first line, add it
            rest = parts[1].strip()
            if rest and rest != 'try':  # Don't duplicate if it's just 'try'
                # This is part of the expression, include it
                pass  # Will be added as part of the full expression below
        else:
            output.append(first_line + '\n')

        # Add all lines from the expression (skipping first which is .op =>)
        for i, line in enumerate(lines_in_impl):
            if i == 0:
                # Skip the ".op =>" part, already added
                # But if there was code after =>, include that
                if '=>' in line:
                    after_arrow = line.split('=>', 1)[1]
                    if after_arrow.strip():
                        output.append(f"                {after_arrow.strip()}\n")
            elif i == len(lines_in_impl) - 1:
                # Last line - handle separately below
                pass
            else:
                output.append(line + '\n')

        # Last line - remove trailing comma if present
        last_line = lines_in_impl[-1].rstrip()
        if last_line.endswith(','):
            last_line = last_line[:-1]
        output.append(last_line + ';\n')  # Add semicolon

        # Add return true and closing
        output.append("                return true;\n")
        output.append("            },\n")
    elif '\n' not in impl:
        # One-liner: .op => expr, OR .op => expr),
        parts = impl.split('=>', 1)
        opcode_part = parts[0].strip()  # Already has leading dot like ".eq"
        code_part = parts[1].strip()
        # Remove trailing comma or ),
        if code_part.endswith('),'):
            code_part = code_part[:-2] + ')'  # Remove , keep )
        elif code_part.endswith(','):
            code_part = code_part[:-1]
        # Don't add extra dot - opcode_part already has it
        output.append(f"            {opcode_part} => {{\n")
        output.append(f"                {code_part};\n")
        output.append(f"                return true;\n")
        output.append(f"            }},\n")
    else:
        # Multi-line case - need to insert return true before final },
        lines_with_newlines = impl.split('\n')

        # Find last }, to insert return true before it
        last_close_idx = -1
        for idx in range(len(lines_with_newlines) - 1, -1, -1):
            if lines_with_newlines[idx].strip() == '},':
                last_close_idx = idx
                break

        for idx, line in enumerate(lines_with_newlines):
            # Insert return true before the final },
            if idx == last_close_idx:
                output.append("                return true;\n")

            output.append(line + '\n')

output.append("            // Return false for opcodes not in shared list\n")
output.append("            else => return false,\n")
output.append("        }\n")
output.append("    }\n")
output.append("\n")

with open('/tmp/emit_common_opcode.zig', 'w') as f:
    f.writelines(output)

print(f"Generated emitCommonOpcode with {len(output)} lines")
print("Wrote to /tmp/emit_common_opcode.zig")
