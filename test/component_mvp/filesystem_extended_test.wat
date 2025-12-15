;; Filesystem Extended Interface Test
;; Tests the 4 new filesystem functions: append-file, read-dir, mkdir, remove-dir
(module
  (import "filesystem" "write-file"
    (func $write_file (param i32 i32 i32 i32) (result i32)))
  (import "filesystem" "append-file"
    (func $append_file (param i32 i32 i32 i32) (result i32)))
  (import "filesystem" "read-dir"
    (func $read_dir (param i32 i32 i32 i32) (result i32)))
  (import "filesystem" "mkdir"
    (func $mkdir (param i32 i32 i32) (result i32)))
  (import "filesystem" "remove-dir"
    (func $remove_dir (param i32 i32 i32) (result i32)))

  (memory (export "memory") 1)

  ;; Data section
  (data (i32.const 0) "/tmp/edgebox-test-dir")       ;; Test directory path
  (data (i32.const 32) "/tmp/edgebox-test-file.txt") ;; Test file path
  (data (i32.const 64) "Hello, World!")              ;; Initial content
  (data (i32.const 80) " Appended text!")            ;; Content to append
  (data (i32.const 96) "/tmp")                       ;; Directory to read
  (data (i32.const 128) "/tmp/edgebox/nested/dir")   ;; Nested directory path
  (data (i32.const 160) "/tmp/edgebox")              ;; Parent directory path

  (func (export "_start")
    (local $error_code i32)
    (local $list_ptr i32)
    (local $list_len i32)

    ;; Test 1: mkdir (create test directory, non-recursive)
    i32.const 0       ;; path_ptr ("/tmp/edgebox-test-dir")
    i32.const 21      ;; path_len
    i32.const 0       ;; recursive = false
    call $mkdir
    local.set $error_code

    ;; Test 2: write-file (create initial file)
    i32.const 32      ;; path_ptr ("/tmp/edgebox-test-file.txt")
    i32.const 29      ;; path_len
    i32.const 64      ;; data_ptr ("Hello, World!")
    i32.const 13      ;; data_len
    call $write_file
    local.set $error_code

    ;; Test 3: append-file (append text to file)
    i32.const 32      ;; path_ptr ("/tmp/edgebox-test-file.txt")
    i32.const 29      ;; path_len
    i32.const 80      ;; data_ptr (" Appended text!")
    i32.const 16      ;; data_len
    call $append_file
    local.set $error_code
    ;; After this, file should contain "Hello, World! Appended text!" (29 bytes)

    ;; Test 4: read-dir (list files in /tmp)
    i32.const 96      ;; path_ptr ("/tmp")
    i32.const 4       ;; path_len
    i32.const 200     ;; out_ptr_ptr (where list pointer will be written)
    i32.const 204     ;; out_len_ptr (where list length will be written)
    call $read_dir
    local.set $error_code

    ;; Load results
    i32.const 200
    i32.load
    local.set $list_ptr

    i32.const 204
    i32.load
    local.set $list_len
    ;; list_len should be > 0 (at least our test file should be in /tmp)

    ;; Test 5: remove-dir (remove test directory, non-recursive - should succeed if empty)
    i32.const 0       ;; path_ptr ("/tmp/edgebox-test-dir")
    i32.const 21      ;; path_len
    i32.const 0       ;; recursive = false
    call $remove_dir
    local.set $error_code

    ;; Test 6: mkdir recursive (create nested directory)
    i32.const 128     ;; path_ptr ("/tmp/edgebox/nested/dir")
    i32.const 23      ;; path_len
    i32.const 1       ;; recursive = true
    call $mkdir
    local.set $error_code

    ;; Test 7: remove-dir recursive (remove nested directory)
    i32.const 160     ;; path_ptr ("/tmp/edgebox")
    i32.const 12      ;; path_len
    i32.const 1       ;; recursive = true
    call $remove_dir
    local.set $error_code

    ;; Success criteria:
    ;; - All error_codes should be 0
    ;; - list_len should be > 0 (directory read succeeded)
    ;; - Directories created and removed successfully
  )
)
