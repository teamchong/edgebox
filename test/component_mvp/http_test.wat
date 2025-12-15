;; HTTP fetch test - simple GET request
(module
  (import "http" "fetch"
    (func $fetch (param i32 i32 i32 i32 i32 i32 i32 i32 i32) (result i32)))

  (memory (export "memory") 1)

  ;; Data section
  (data (i32.const 0) "https://httpbin.org/get")  ;; URL (23 bytes)

  (func (export "_start")
    (local $error_code i32)
    (local $status i32)
    (local $ok i32)
    (local $body_ptr i32)
    (local $body_len i32)

    ;; Call fetch with simple GET request (no headers, no body)
    ;; Parameters: url_ptr, url_len, method(GET=0), headers_ptr, headers_count,
    ;;             body_ptr, body_len, timeout_ms, out_response_ptr
    i32.const 0       ;; url_ptr
    i32.const 23      ;; url_len
    i32.const 0       ;; method (0=GET)
    i32.const 0       ;; headers_ptr (no headers)
    i32.const 0       ;; headers_count
    i32.const 0       ;; body_ptr (no body)
    i32.const 0       ;; body_len
    i32.const 5000    ;; timeout_ms
    i32.const 128     ;; out_response_ptr (write HttpResponse here)
    call $fetch
    local.set $error_code

    ;; Load HttpResponse fields from memory (24 bytes total)
    ;; Offset 0: status (u16, padded to 4 bytes)
    i32.const 128
    i32.load
    local.set $status

    ;; Offset 4: ok (u32, bool as 0/1)
    i32.const 132
    i32.load
    local.set $ok

    ;; Offset 8: body_ptr (u32)
    i32.const 136
    i32.load
    local.set $body_ptr

    ;; Offset 12: body_len (u32)
    i32.const 140
    i32.load
    local.set $body_len

    ;; Success criteria:
    ;; - error_code should be 0
    ;; - status should be 200
    ;; - ok should be 1
    ;; - body_len should be > 0
    ;; - body_ptr should be non-zero

    ;; Results are in locals, WASM module can verify or print them
    ;; For now, execution completing successfully is the test
  )
)
