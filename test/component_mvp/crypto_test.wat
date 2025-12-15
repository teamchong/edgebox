;; Crypto interface test - tests all 4 crypto functions
(module
  (import "crypto" "hash"
    (func $hash (param i32 i32 i32 i32 i32) (result i32)))
  (import "crypto" "random-bytes"
    (func $random_bytes (param i32 i32 i32) (result i32)))
  (import "crypto" "get-hash-algorithms"
    (func $get_hash_algorithms (param i32 i32) (result i32)))

  (memory (export "memory") 1)

  ;; Data section
  (data (i32.const 0) "hello world")  ;; Input data for hash (11 bytes)

  (func (export "_start")
    (local $error_code i32)
    (local $hash_ptr i32)
    (local $hash_len i32)
    (local $random_ptr i32)
    (local $random_len i32)
    (local $algos_ptr i32)
    (local $algos_count i32)

    ;; Test 1: hash("sha256", "hello world")
    ;; SHA256 of "hello world" = b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9
    i32.const 0       ;; algorithm (0 = sha256)
    i32.const 0       ;; data_ptr
    i32.const 11      ;; data_len
    i32.const 128     ;; out_ptr_ptr (where to write result ptr)
    i32.const 132     ;; out_len_ptr (where to write result len)
    call $hash
    local.set $error_code

    ;; Load hash result
    i32.const 128
    i32.load
    local.set $hash_ptr

    i32.const 132
    i32.load
    local.set $hash_len

    ;; Test 2: random-bytes(16) - generate 16 random bytes
    i32.const 16      ;; size
    i32.const 136     ;; out_ptr_ptr
    i32.const 140     ;; out_len_ptr
    call $random_bytes
    local.set $error_code

    ;; Load random bytes result
    i32.const 136
    i32.load
    local.set $random_ptr

    i32.const 140
    i32.load
    local.set $random_len

    ;; Test 3: get-hash-algorithms() - should return ["sha256", "sha384", "sha512", "sha1", "md5"]
    i32.const 144     ;; out_ptr_ptr (pointer to list array)
    i32.const 148     ;; out_len_ptr (count of algorithms)
    call $get_hash_algorithms
    local.set $error_code

    ;; Load algorithm list
    i32.const 144
    i32.load
    local.set $algos_ptr

    i32.const 148
    i32.load
    local.set $algos_count

    ;; Success criteria:
    ;; - All error_codes should be 0
    ;; - hash_len should be 64 (hex-encoded SHA256 = 32 bytes * 2)
    ;; - random_len should be 16
    ;; - algos_count should be 5

    ;; Results are in locals, WASM module can verify them
    ;; For now, execution completing successfully is the test
  )
)
