;; Test filesystem read-file (output string allocation)
(module
  (import "filesystem" "read-file"
    (func $read_file (param i32 i32 i32 i32 i32) (result i32)))

  (memory (export "memory") 1)

  (data (i32.const 0) "/tmp/edgebox_test.txt")

  (func (export "_start")
    (local $error_code i32)
    (local $content_ptr i32)
    (local $content_len i32)

    ;; Read file
    i32.const 0      ;; path_ptr
    i32.const 21     ;; path_len
    i32.const 0      ;; encoding: UTF-8
    i32.const 128    ;; out_ptr_ptr (where to write result ptr)
    i32.const 132    ;; out_len_ptr (where to write result len)
    call $read_file
    local.set $error_code

    ;; Load result ptr and len from memory
    i32.const 128
    i32.load
    local.set $content_ptr

    i32.const 132
    i32.load
    local.set $content_len

    ;; Results: error_code should be 0, content_len should be 36
    ;; content_ptr should point to allocated memory with file contents
  )
)
