;; Component Model MVP Test - Timer Interface
;; Tests importing and calling timer::set-timeout from WASM
;;
;; This is a minimal proof-of-concept that demonstrates WAMR can call
;; Component Model interfaces through the import resolver bridge.

(module
  ;; Import timer::set-timeout from Component Model
  ;; Component Model signature: set-timeout: func(delay: u32) -> u64
  ;; WAMR signature: (i)I  (i32 → i64)
  (import "timer" "set-timeout" (func $set_timeout (param i32) (result i64)))

  ;; Import timer::clear-timeout from Component Model
  ;; Component Model signature: clear-timeout: func(id: u64)
  ;; WAMR signature: (I)  (i64 → void)
  (import "timer" "clear-timeout" (func $clear_timeout (param i64)))

  ;; Import timer::set-interval from Component Model
  ;; Component Model signature: set-interval: func(delay: u32) -> u64
  ;; WAMR signature: (i)I  (i32 → i64)
  (import "timer" "set-interval" (func $set_interval (param i32) (result i64)))

  ;; Memory (required by WAMR for WASI modules)
  (memory (export "memory") 1)

  ;; Export: Test set-timeout with 1000ms delay
  ;; Returns: timer ID (u64) or 0 on error
  (func (export "test_set_timeout") (result i64)
    ;; Call set-timeout with 1000ms delay
    i32.const 1000
    call $set_timeout
  )

  ;; Export: Test set-interval with 500ms delay
  ;; Returns: interval ID (u64) or 0 on error
  (func (export "test_set_interval") (result i64)
    ;; Call set-interval with 500ms delay
    i32.const 500
    call $set_interval
  )

  ;; Export: Test clear-timeout
  ;; Params: timer_id (u64)
  (func (export "test_clear_timeout") (param $timer_id i64)
    ;; Clear the given timer
    local.get $timer_id
    call $clear_timeout
  )

  ;; Export: Full workflow test
  ;; Returns: 1 if successful, 0 if error
  (func (export "test_full_workflow") (result i32)
    (local $timer_id i64)
    (local $interval_id i64)

    ;; 1. Create a timeout
    i32.const 2000
    call $set_timeout
    local.set $timer_id

    ;; Check if timer creation succeeded (non-zero ID)
    local.get $timer_id
    i64.const 0
    i64.eq
    if
      ;; Timer creation failed
      i32.const 0
      return
    end

    ;; 2. Create an interval
    i32.const 1000
    call $set_interval
    local.set $interval_id

    ;; Check if interval creation succeeded
    local.get $interval_id
    i64.const 0
    i64.eq
    if
      ;; Interval creation failed
      i32.const 0
      return
    end

    ;; 3. Clear the timer
    local.get $timer_id
    call $clear_timeout

    ;; 4. Clear the interval
    local.get $interval_id
    call $clear_timeout

    ;; Success!
    i32.const 1
  )

  ;; Entry point for WASM execution
  (func (export "_start")
    (local $timer_id i64)
    (local $result i32)

    ;; Test set-timeout - should return non-zero timer ID
    i32.const 1000
    call $set_timeout
    local.set $timer_id

    ;; Clear the timer
    local.get $timer_id
    call $clear_timeout

    ;; Note: WASM has no print() - results verified by not crashing
  )
)
