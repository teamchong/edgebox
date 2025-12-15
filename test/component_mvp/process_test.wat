;; Test process exec - simple command execution
(module
  (import "process" "exec"
    (func $exec (param i32 i32 i32 i32 i32 i32 i32 i32) (result i32)))

  (memory (export "memory") 1)

  ;; Data section
  (data (i32.const 0) "echo")         ;; program (4 bytes)
  (data (i32.const 16) "hello")       ;; arg (5 bytes)

  (func (export "_start")
    (local $error_code i32)
    (local $exit_code i32)
    (local $stdout_ptr i32)
    (local $stdout_len i32)

    ;; Setup args array at offset 32
    ;; args[0] = pointer to "hello" string (offset 16)
    i32.const 32
    i32.const 16
    i32.store

    ;; Setup args_len array at offset 36
    ;; args_len[0] = length of "hello" (5 bytes)
    i32.const 36
    i32.const 5
    i32.store

    ;; Call exec: prog_ptr, prog_len, args_ptr, args_count,
    ;;            env_ptr, env_count, timeout_ms, out_result_ptr
    i32.const 0       ;; prog_ptr ("echo")
    i32.const 4       ;; prog_len
    i32.const 32      ;; args_ptr (pointer to args array with ptr+len pairs)
    i32.const 1       ;; args_count
    i32.const 0       ;; env_ptr (no env vars)
    i32.const 0       ;; env_count
    i32.const 5000    ;; timeout_ms
    i32.const 128     ;; out_result_ptr
    call $exec
    local.set $error_code

    ;; Load ExecResult fields from memory (24 bytes total)
    ;; Offset 0: exit_code (i32)
    i32.const 128
    i32.load
    local.set $exit_code

    ;; Offset 4: stdout_ptr (u32)
    i32.const 132
    i32.load
    local.set $stdout_ptr

    ;; Offset 8: stdout_len (u32)
    i32.const 136
    i32.load
    local.set $stdout_len

    ;; Success criteria:
    ;; - error_code should be 0
    ;; - exit_code should be 0
    ;; - stdout_len should be > 0 (echo outputs "hello\n")
    ;; - stdout_ptr should be non-zero

    ;; Results are in locals, WASM module can verify or print them
    ;; For now, execution completing successfully is the test
  )
)
