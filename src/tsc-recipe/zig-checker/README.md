# Zig Type Checker (Recipe)

Incremental Zig type checker for TypeScript. Handles simple cases natively,
delegates complex cases to TSC. Each piece is small, tested, and correct.

## Architecture

```
Source .ts files
  → Zig Tokenizer (bytes → tokens)
  → Zig Parser (tokens → typed AST)
  → Zig Checker (AST → diagnostics for simple cases)
  → TSC Checker (AST → diagnostics for complex cases)
  → Merge diagnostics
```

## Pieces (in order of implementation)

1. **Tokenizer** — scan bytes into tokens (keywords, identifiers, literals, operators)
2. **Import resolver** — resolve `from '...'` to file paths
3. **Type annotation parser** — extract `: number`, `: string`, etc.
4. **Literal type checker** — `const x: number = "hello"` → error
5. **Function signature checker** — argument count/type matching
6. **Structural checker** — interface member comparison

Each piece is compiled to WASM and loaded via the recipe system.
