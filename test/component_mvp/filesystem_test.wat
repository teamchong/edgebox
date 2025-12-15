;; Component Model Phase 8b - Filesystem Interface Test
;; Tests importing and calling filesystem functions from WASM
;;
;; This validates ABI lowering for complex types:
;; - String inputs (ptr + len)
;; - String outputs (allocated in WASM memory)
;; - Result<T, E> encoding (0 = success, >0 = error code)
;; - Record types (FileStat)

(module
  ;; Import filesystem functions from Component Model
  (import "filesystem" "exists"
    (func $exists (param i32 i32) (result i32)))
  (import "filesystem" "write-file"
    (func $write_file (param i32 i32 i32 i32) (result i32)))
  (import "filesystem" "read-file"
    (func $read_file (param i32 i32 i32 i32 i32) (result i32)))
  (import "filesystem" "stat"
    (func $stat (param i32 i32 i32) (result i32)))
  (import "filesystem" "remove-file"
    (func $remove_file (param i32 i32) (result i32)))
  (import "filesystem" "rename"
    (func $rename (param i32 i32 i32 i32) (result i32)))
  (import "filesystem" "copy-file"
    (func $copy_file (param i32 i32 i32 i32) (result i32)))

  ;; Memory (required for storing strings and results)
  (memory (export "memory") 2)

  ;; Test data in memory
  (data (i32.const 0) "/tmp/test.txt")           ;; 13 bytes
  (data (i32.const 32) "Hello, Component Model!") ;; 23 bytes
  (data (i32.const 64) "/tmp/test2.txt")          ;; 14 bytes
  (data (i32.const 96) "/tmp/test_copy.txt")      ;; 18 bytes

  ;; Memory layout:
  ;; 0-31:    /tmp/test.txt (path 1)
  ;; 32-63:   Hello, Component Model! (data)
  ;; 64-95:   /tmp/test2.txt (path 2)
  ;; 96-127:  /tmp/test_copy.txt (path 3)
  ;; 128-159: Output buffer for read-file (ptr, len)
  ;; 160-199: FileStat buffer (40 bytes)
  ;; 200+:    Dynamic allocations from Component Model

  ;; Export: Test write-file
  ;; Writes "Hello, Component Model!" to /tmp/test.txt
  ;; Returns: 0 on success, error code on failure
  (func (export "test_write_file") (result i32)
    i32.const 0      ;; path_ptr: /tmp/test.txt
    i32.const 13     ;; path_len
    i32.const 32     ;; data_ptr: Hello, Component Model!
    i32.const 23     ;; data_len
    call $write_file
  )

  ;; Export: Test read-file
  ;; Reads /tmp/test.txt and returns content length
  ;; Returns: content length on success, -1 on error
  (func (export "test_read_file") (result i32)
    (local $error_code i32)
    (local $content_ptr i32)
    (local $content_len i32)

    ;; Call read-file
    i32.const 0      ;; path_ptr: /tmp/test.txt
    i32.const 13     ;; path_len
    i32.const 0      ;; encoding: UTF-8
    i32.const 128    ;; out_ptr_ptr (where to write result ptr)
    i32.const 132    ;; out_len_ptr (where to write result len)
    call $read_file
    local.set $error_code

    ;; Check error
    local.get $error_code
    i32.const 0
    i32.ne
    if
      i32.const -1
      return
    end

    ;; Read result len from memory
    i32.const 132
    i32.load
    return
  )

  ;; Export: Test exists
  ;; Checks if /tmp/test.txt exists
  ;; Returns: 1 if exists, 0 if not
  (func (export "test_exists") (result i32)
    i32.const 0      ;; path_ptr: /tmp/test.txt
    i32.const 13     ;; path_len
    call $exists
  )

  ;; Export: Test stat
  ;; Gets file stats for /tmp/test.txt
  ;; Returns: file size on success, -1 on error
  (func (export "test_stat") (result i32)
    (local $error_code i32)
    (local $file_size i64)

    ;; Call stat
    i32.const 0      ;; path_ptr: /tmp/test.txt
    i32.const 13     ;; path_len
    i32.const 160    ;; out_stat_ptr (FileStat buffer)
    call $stat
    local.set $error_code

    ;; Check error
    local.get $error_code
    i32.const 0
    i32.ne
    if
      i32.const -1
      return
    end

    ;; Read file size from FileStat (first 8 bytes)
    i32.const 160
    i64.load
    i32.wrap_i64
    return
  )

  ;; Export: Test remove-file
  ;; Deletes /tmp/test.txt
  ;; Returns: 0 on success, error code on failure
  (func (export "test_remove_file") (result i32)
    i32.const 0      ;; path_ptr: /tmp/test.txt
    i32.const 13     ;; path_len
    call $remove_file
  )

  ;; Export: Test rename
  ;; Renames /tmp/test.txt to /tmp/test2.txt
  ;; Returns: 0 on success, error code on failure
  (func (export "test_rename") (result i32)
    i32.const 0      ;; old_path_ptr: /tmp/test.txt
    i32.const 13     ;; old_path_len
    i32.const 64     ;; new_path_ptr: /tmp/test2.txt
    i32.const 14     ;; new_path_len
    call $rename
  )

  ;; Export: Test copy-file
  ;; Copies /tmp/test.txt to /tmp/test_copy.txt
  ;; Returns: 0 on success, error code on failure
  (func (export "test_copy_file") (result i32)
    i32.const 0      ;; src_ptr: /tmp/test.txt
    i32.const 13     ;; src_len
    i32.const 96     ;; dest_ptr: /tmp/test_copy.txt
    i32.const 18     ;; dest_len
    call $copy_file
  )

  ;; Export: Full workflow test
  ;; Tests the complete filesystem flow
  ;; Returns: 1 if all tests pass, 0 if any fail
  (func $test_full_workflow_impl (result i32)
    (local $result i32)
    (local $error_code i32)

    ;; 1. Write file
    i32.const 0      ;; path_ptr: /tmp/test.txt
    i32.const 13     ;; path_len
    i32.const 32     ;; data_ptr: Hello, Component Model!
    i32.const 23     ;; data_len
    call $write_file
    i32.const 0
    i32.ne
    if
      ;; Write failed
      i32.const 0
      return
    end

    ;; 2. Check file exists
    i32.const 0      ;; path_ptr: /tmp/test.txt
    i32.const 13     ;; path_len
    call $exists
    i32.const 1
    i32.ne
    if
      ;; File doesn't exist after write
      i32.const 0
      return
    end

    ;; 3. Read file back
    i32.const 0      ;; path_ptr: /tmp/test.txt
    i32.const 13     ;; path_len
    i32.const 0      ;; encoding: UTF-8
    i32.const 128    ;; out_ptr_ptr
    i32.const 132    ;; out_len_ptr
    call $read_file
    local.set $error_code
    local.get $error_code
    i32.const 0
    i32.ne
    if
      ;; Read failed
      i32.const 0
      return
    end
    ;; Check content length
    i32.const 132
    i32.load
    i32.const 23
    i32.ne
    if
      ;; Content length mismatch
      i32.const 0
      return
    end

    ;; 4. Get file stats
    i32.const 0      ;; path_ptr: /tmp/test.txt
    i32.const 13     ;; path_len
    i32.const 160    ;; out_stat_ptr
    call $stat
    local.set $error_code
    local.get $error_code
    i32.const 0
    i32.ne
    if
      ;; Stat failed
      i32.const 0
      return
    end
    ;; Check file size
    i32.const 160
    i64.load
    i64.const 23
    i64.ne
    if
      ;; File size mismatch
      i32.const 0
      return
    end

    ;; 5. Copy file
    i32.const 0      ;; src_ptr: /tmp/test.txt
    i32.const 13     ;; src_len
    i32.const 96     ;; dest_ptr: /tmp/test_copy.txt
    i32.const 18     ;; dest_len
    call $copy_file
    i32.const 0
    i32.ne
    if
      ;; Copy failed
      i32.const 0
      return
    end

    ;; 6. Check copy exists
    i32.const 96     ;; path_ptr: /tmp/test_copy.txt
    i32.const 18     ;; path_len
    call $exists
    i32.const 1
    i32.ne
    if
      ;; Copy doesn't exist
      i32.const 0
      return
    end

    ;; 7. Rename original file
    i32.const 0      ;; old_path_ptr: /tmp/test.txt
    i32.const 13     ;; old_path_len
    i32.const 64     ;; new_path_ptr: /tmp/test2.txt
    i32.const 14     ;; new_path_len
    call $rename
    i32.const 0
    i32.ne
    if
      ;; Rename failed
      i32.const 0
      return
    end

    ;; 8. Check old path doesn't exist
    i32.const 0      ;; path_ptr: /tmp/test.txt (old)
    i32.const 13     ;; path_len
    call $exists
    i32.const 0
    i32.ne
    if
      ;; Old path still exists after rename
      i32.const 0
      return
    end

    ;; 9. Check new path exists
    i32.const 64     ;; path_ptr: /tmp/test2.txt (new)
    i32.const 14     ;; path_len
    call $exists
    i32.const 1
    i32.ne
    if
      ;; New path doesn't exist after rename
      i32.const 0
      return
    end

    ;; 10. Delete renamed file
    i32.const 64     ;; path_ptr: /tmp/test2.txt
    i32.const 14     ;; path_len
    call $remove_file
    i32.const 0
    i32.ne
    if
      ;; Delete failed
      i32.const 0
      return
    end

    ;; 11. Delete copied file
    i32.const 96     ;; path_ptr: /tmp/test_copy.txt
    i32.const 18     ;; path_len
    call $remove_file
    i32.const 0
    i32.ne
    if
      ;; Delete failed
      i32.const 0
      return
    end

    ;; All tests passed!
    i32.const 1
  )

  (func (export "test_full_workflow") (result i32)
    call $test_full_workflow_impl
  )

  ;; Entry point for WASM execution
  (func (export "_start")
    (local $result i32)

    ;; Run full workflow test
    call $test_full_workflow_impl
    local.set $result

    ;; If result is 0, test failed (but we don't crash, just return)
    ;; Note: WASM has no print() - results verified by exit code or memory inspection
  )
)
