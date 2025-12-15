;; Simple filesystem test - just write and verify file exists
(module
  (import "filesystem" "write-file"
    (func $write_file (param i32 i32 i32 i32) (result i32)))
  (import "filesystem" "exists"
    (func $exists (param i32 i32) (result i32)))

  (memory (export "memory") 1)

  (data (i32.const 0) "/tmp/edgebox_test.txt")
  (data (i32.const 32) "Hello from EdgeBox Component Model!")

  (func (export "_start")
    (local $write_result i32)
    (local $exists_result i32)

    ;; Write file
    i32.const 0      ;; path_ptr
    i32.const 21     ;; path_len
    i32.const 32     ;; data_ptr
    i32.const 36     ;; data_len
    call $write_file
    local.set $write_result

    ;; Check if file exists
    i32.const 0      ;; path_ptr
    i32.const 21     ;; path_len
    call $exists
    local.set $exists_result

    ;; Results are in locals but we can't print them
    ;; Success = write_result is 0 and exists_result is 1
  )
)
