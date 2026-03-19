// V8 linking stubs — provides no-op implementations for Rust callbacks
// that librusty_v8.a expects from Deno's Rust runtime.
//
// These are never called during normal V8 operation (init, eval, snapshots).
// They exist solely to satisfy the linker. Categories:
//   1. RustObj (cppgc garbage-collected Rust objects) — not used
//   2. CustomPlatform callbacks — we use DefaultPlatform, not custom
//   3. V8 Inspector callbacks — no debugger yet
//   4. ValueSerializer/Deserializer — no structured clone yet
//   5. temporal_rs (TC39 Temporal API in Rust) — not needed for TSC
//
// Generated from: zig build v8-test 2>&1 | grep 'undefined symbol'

#include <stddef.h>
#include <stdint.h>

// =============================================================================
// 1. RustObj stubs (cppgc integration)
// =============================================================================
void rusty_v8_RustObj_drop(void* self) { (void)self; }
void rusty_v8_RustObj_trace(const void* self, void* visitor) { (void)self; (void)visitor; }
const char* rusty_v8_RustObj_get_name(const void* self) { (void)self; return ""; }

// =============================================================================
// 2. CustomPlatform callbacks (we use DefaultPlatform)
// =============================================================================
void v8__Platform__CustomPlatform__BASE__PostTask(void* ctx, void* isolate, void* task) { (void)ctx; (void)isolate; (void)task; }
void v8__Platform__CustomPlatform__BASE__PostNonNestableTask(void* ctx, void* isolate, void* task) { (void)ctx; (void)isolate; (void)task; }
void v8__Platform__CustomPlatform__BASE__PostDelayedTask(void* ctx, void* isolate, void* task, double delay) { (void)ctx; (void)isolate; (void)task; (void)delay; }
void v8__Platform__CustomPlatform__BASE__PostNonNestableDelayedTask(void* ctx, void* isolate, void* task, double delay) { (void)ctx; (void)isolate; (void)task; (void)delay; }
void v8__Platform__CustomPlatform__BASE__PostIdleTask(void* ctx, void* isolate, void* task) { (void)ctx; (void)isolate; (void)task; }
void v8__Platform__CustomPlatform__BASE__DROP(void* ctx) { (void)ctx; }

// =============================================================================
// 3. V8 Inspector callbacks (no debugger)
// =============================================================================
void v8_inspector__V8Inspector__Channel__BASE__sendResponse(void* self, int call_id, void* message) { (void)self; (void)call_id; (void)message; }
void v8_inspector__V8Inspector__Channel__BASE__sendNotification(void* self, void* message) { (void)self; (void)message; }
void v8_inspector__V8Inspector__Channel__BASE__flushProtocolNotifications(void* self) { (void)self; }
void v8_inspector__V8InspectorClient__BASE__runMessageLoopOnPause(void* self, int group_id) { (void)self; (void)group_id; }
void v8_inspector__V8InspectorClient__BASE__quitMessageLoopOnPause(void* self) { (void)self; }
void v8_inspector__V8InspectorClient__BASE__runIfWaitingForDebugger(void* self, int group_id) { (void)self; (void)group_id; }
void v8_inspector__V8InspectorClient__BASE__ensureDefaultContextInGroup(void* self, int group_id) { (void)self; (void)group_id; }
void v8_inspector__V8InspectorClient__BASE__consoleAPIMessage(void* self, int group_id, int level, void* message, void* url, unsigned line, unsigned col, void* stack) { (void)self; (void)group_id; (void)level; (void)message; (void)url; (void)line; (void)col; (void)stack; }
void v8_inspector__V8InspectorClient__BASE__resourceNameToUrl(void* self, void* name) { (void)self; (void)name; }
int64_t v8_inspector__V8InspectorClient__BASE__generateUniqueId(void* self) { (void)self; return 0; }

// =============================================================================
// 4. ValueSerializer/Deserializer callbacks (no structured clone)
// =============================================================================
void v8__ValueSerializer__Delegate__ThrowDataCloneError(void* self, void* message) { (void)self; (void)message; }
int v8__ValueSerializer__Delegate__HasCustomHostObject(void* self, void* isolate) { (void)self; (void)isolate; return 0; }
int v8__ValueSerializer__Delegate__IsHostObject(void* self, void* isolate, void* object) { (void)self; (void)isolate; (void)object; return 0; }
int v8__ValueSerializer__Delegate__WriteHostObject(void* self, void* isolate, void* object) { (void)self; (void)isolate; (void)object; return 0; }
int v8__ValueSerializer__Delegate__GetSharedArrayBufferId(void* self, void* isolate, void* sab) { (void)self; (void)isolate; (void)sab; return 0; }
int v8__ValueSerializer__Delegate__GetWasmModuleTransferId(void* self, void* isolate, void* module) { (void)self; (void)isolate; (void)module; return 0; }
void* v8__ValueSerializer__Delegate__ReallocateBufferMemory(void* self, void* old_buf, size_t size, size_t* actual) { (void)self; (void)old_buf; (void)size; if (actual) *actual = 0; return NULL; }
void v8__ValueSerializer__Delegate__FreeBufferMemory(void* self, void* buf) { (void)self; (void)buf; }
void* v8__ValueDeserializer__Delegate__ReadHostObject(void* self, void* isolate) { (void)self; (void)isolate; return NULL; }
void* v8__ValueDeserializer__Delegate__GetWasmModuleFromId(void* self, void* isolate, uint32_t id) { (void)self; (void)isolate; (void)id; return NULL; }
void* v8__ValueDeserializer__Delegate__GetSharedArrayBufferFromId(void* self, void* isolate, uint32_t id) { (void)self; (void)isolate; (void)id; return NULL; }

// =============================================================================
// 5. temporal_rs stubs (TC39 Temporal API — not needed for TSC)
// All temporal_rs functions are Rust FFI exports that V8 calls for Temporal.
// We provide void/zero stubs; Temporal.* constructors will fail gracefully.
// =============================================================================

// Generic result struct for temporal operations (matches Rust FFI layout)
// Most temporal_rs functions return result structs — zero-init is safe (= error)

void temporal_rs_AnyCalendarKind_get_for_str(void) {}
void temporal_rs_AnyCalendarKind_parse_temporal_calendar_string(void) {}
void temporal_rs_Calendar_identifier(void) {}
void temporal_rs_Calendar_kind(void) {}

// Duration
void temporal_rs_Duration_abs(void) {}
void temporal_rs_Duration_add(void) {}
void temporal_rs_Duration_clone(void) {}
void temporal_rs_Duration_compare_with_provider(void) {}
void temporal_rs_Duration_create(void) {}
void temporal_rs_Duration_days(void) {}
void temporal_rs_Duration_destroy(void) {}
void temporal_rs_Duration_from_partial_duration(void) {}
void temporal_rs_Duration_from_utf16(void) {}
void temporal_rs_Duration_from_utf8(void) {}
void temporal_rs_Duration_hours(void) {}
void temporal_rs_Duration_microseconds(void) {}
void temporal_rs_Duration_milliseconds(void) {}
void temporal_rs_Duration_minutes(void) {}
void temporal_rs_Duration_months(void) {}
void temporal_rs_Duration_nanoseconds(void) {}
void temporal_rs_Duration_negated(void) {}
void temporal_rs_Duration_round_with_provider(void) {}
void temporal_rs_Duration_seconds(void) {}
void temporal_rs_Duration_sign(void) {}
void temporal_rs_Duration_subtract(void) {}
void temporal_rs_Duration_to_string(void) {}
void temporal_rs_Duration_total_with_provider(void) {}
void temporal_rs_Duration_weeks(void) {}
void temporal_rs_Duration_years(void) {}

// I128Nanoseconds
void temporal_rs_I128Nanoseconds_is_valid(void) {}

// Instant
void temporal_rs_Instant_add(void) {}
void temporal_rs_Instant_clone(void) {}
void temporal_rs_Instant_compare(void) {}
void temporal_rs_Instant_destroy(void) {}
void temporal_rs_Instant_epoch_milliseconds(void) {}
void temporal_rs_Instant_epoch_nanoseconds(void) {}
void temporal_rs_Instant_from_epoch_milliseconds(void) {}
void temporal_rs_Instant_from_utf16(void) {}
void temporal_rs_Instant_from_utf8(void) {}
void temporal_rs_Instant_round(void) {}
void temporal_rs_Instant_since(void) {}
void temporal_rs_Instant_subtract(void) {}
void temporal_rs_Instant_to_ixdtf_string_with_provider(void) {}
void temporal_rs_Instant_to_zoned_date_time_iso_with_provider(void) {}
void temporal_rs_Instant_try_new(void) {}
void temporal_rs_Instant_until(void) {}

// OwnedRelativeTo
void temporal_rs_OwnedRelativeTo_from_utf16_with_provider(void) {}
void temporal_rs_OwnedRelativeTo_from_utf8_with_provider(void) {}

// ParsedDate
void temporal_rs_ParsedDate_destroy(void) {}
void temporal_rs_ParsedDate_from_utf16(void) {}
void temporal_rs_ParsedDate_from_utf8(void) {}
void temporal_rs_ParsedDate_month_day_from_utf16(void) {}
void temporal_rs_ParsedDate_month_day_from_utf8(void) {}
void temporal_rs_ParsedDate_year_month_from_utf16(void) {}
void temporal_rs_ParsedDate_year_month_from_utf8(void) {}

// ParsedDateTime
void temporal_rs_ParsedDateTime_destroy(void) {}
void temporal_rs_ParsedDateTime_from_utf16(void) {}
void temporal_rs_ParsedDateTime_from_utf8(void) {}

// ParsedZonedDateTime
void temporal_rs_ParsedZonedDateTime_destroy(void) {}
void temporal_rs_ParsedZonedDateTime_from_utf16_with_provider(void) {}
void temporal_rs_ParsedZonedDateTime_from_utf8_with_provider(void) {}

// PlainDate
void temporal_rs_PlainDate_add(void) {}
void temporal_rs_PlainDate_calendar(void) {}
void temporal_rs_PlainDate_clone(void) {}
void temporal_rs_PlainDate_compare(void) {}
void temporal_rs_PlainDate_day(void) {}
void temporal_rs_PlainDate_day_of_week(void) {}
void temporal_rs_PlainDate_day_of_year(void) {}
void temporal_rs_PlainDate_days_in_month(void) {}
void temporal_rs_PlainDate_days_in_week(void) {}
void temporal_rs_PlainDate_days_in_year(void) {}
void temporal_rs_PlainDate_destroy(void) {}
void temporal_rs_PlainDate_equals(void) {}
void temporal_rs_PlainDate_era(void) {}
void temporal_rs_PlainDate_era_year(void) {}
void temporal_rs_PlainDate_from_parsed(void) {}
void temporal_rs_PlainDate_from_partial(void) {}
void temporal_rs_PlainDate_in_leap_year(void) {}
void temporal_rs_PlainDate_month(void) {}
void temporal_rs_PlainDate_month_code(void) {}
void temporal_rs_PlainDate_months_in_year(void) {}
void temporal_rs_PlainDate_since(void) {}
void temporal_rs_PlainDate_subtract(void) {}
void temporal_rs_PlainDate_to_ixdtf_string(void) {}
void temporal_rs_PlainDate_to_plain_date_time(void) {}
void temporal_rs_PlainDate_to_plain_month_day(void) {}
void temporal_rs_PlainDate_to_plain_year_month(void) {}
void temporal_rs_PlainDate_to_zoned_date_time_with_provider(void) {}
void temporal_rs_PlainDate_try_new(void) {}
void temporal_rs_PlainDate_until(void) {}
void temporal_rs_PlainDate_week_of_year(void) {}
void temporal_rs_PlainDate_with(void) {}
void temporal_rs_PlainDate_with_calendar(void) {}
void temporal_rs_PlainDate_year(void) {}
void temporal_rs_PlainDate_year_of_week(void) {}

// PlainDateTime
void temporal_rs_PlainDateTime_add(void) {}
void temporal_rs_PlainDateTime_calendar(void) {}
void temporal_rs_PlainDateTime_clone(void) {}
void temporal_rs_PlainDateTime_compare(void) {}
void temporal_rs_PlainDateTime_day(void) {}
void temporal_rs_PlainDateTime_day_of_week(void) {}
void temporal_rs_PlainDateTime_day_of_year(void) {}
void temporal_rs_PlainDateTime_days_in_month(void) {}
void temporal_rs_PlainDateTime_days_in_week(void) {}
void temporal_rs_PlainDateTime_days_in_year(void) {}
void temporal_rs_PlainDateTime_destroy(void) {}
void temporal_rs_PlainDateTime_equals(void) {}
void temporal_rs_PlainDateTime_era(void) {}
void temporal_rs_PlainDateTime_era_year(void) {}
void temporal_rs_PlainDateTime_from_parsed(void) {}
void temporal_rs_PlainDateTime_from_partial(void) {}
void temporal_rs_PlainDateTime_hour(void) {}
void temporal_rs_PlainDateTime_in_leap_year(void) {}
void temporal_rs_PlainDateTime_microsecond(void) {}
void temporal_rs_PlainDateTime_millisecond(void) {}
void temporal_rs_PlainDateTime_minute(void) {}
void temporal_rs_PlainDateTime_month(void) {}
void temporal_rs_PlainDateTime_month_code(void) {}
void temporal_rs_PlainDateTime_months_in_year(void) {}
void temporal_rs_PlainDateTime_nanosecond(void) {}
void temporal_rs_PlainDateTime_round(void) {}
void temporal_rs_PlainDateTime_second(void) {}
void temporal_rs_PlainDateTime_since(void) {}
void temporal_rs_PlainDateTime_subtract(void) {}
void temporal_rs_PlainDateTime_to_ixdtf_string(void) {}
void temporal_rs_PlainDateTime_to_plain_date(void) {}
void temporal_rs_PlainDateTime_to_plain_time(void) {}
void temporal_rs_PlainDateTime_to_zoned_date_time_with_provider(void) {}
void temporal_rs_PlainDateTime_try_new(void) {}
void temporal_rs_PlainDateTime_until(void) {}
void temporal_rs_PlainDateTime_week_of_year(void) {}
void temporal_rs_PlainDateTime_with(void) {}
void temporal_rs_PlainDateTime_with_calendar(void) {}
void temporal_rs_PlainDateTime_with_time(void) {}
void temporal_rs_PlainDateTime_year(void) {}
void temporal_rs_PlainDateTime_year_of_week(void) {}

// PlainMonthDay
void temporal_rs_PlainMonthDay_calendar(void) {}
void temporal_rs_PlainMonthDay_clone(void) {}
void temporal_rs_PlainMonthDay_day(void) {}
void temporal_rs_PlainMonthDay_destroy(void) {}
void temporal_rs_PlainMonthDay_epoch_ms_for_with_provider(void) {}
void temporal_rs_PlainMonthDay_equals(void) {}
void temporal_rs_PlainMonthDay_from_parsed(void) {}
void temporal_rs_PlainMonthDay_from_partial(void) {}
void temporal_rs_PlainMonthDay_month_code(void) {}
void temporal_rs_PlainMonthDay_to_ixdtf_string(void) {}
void temporal_rs_PlainMonthDay_to_plain_date(void) {}
void temporal_rs_PlainMonthDay_try_new_with_overflow(void) {}
void temporal_rs_PlainMonthDay_with(void) {}

// PlainTime
void temporal_rs_PlainTime_add(void) {}
void temporal_rs_PlainTime_clone(void) {}
void temporal_rs_PlainTime_compare(void) {}
void temporal_rs_PlainTime_destroy(void) {}
void temporal_rs_PlainTime_equals(void) {}
void temporal_rs_PlainTime_from_partial(void) {}
void temporal_rs_PlainTime_from_utf16(void) {}
void temporal_rs_PlainTime_from_utf8(void) {}
void temporal_rs_PlainTime_hour(void) {}
void temporal_rs_PlainTime_microsecond(void) {}
void temporal_rs_PlainTime_millisecond(void) {}
void temporal_rs_PlainTime_minute(void) {}
void temporal_rs_PlainTime_nanosecond(void) {}
void temporal_rs_PlainTime_round(void) {}
void temporal_rs_PlainTime_second(void) {}
void temporal_rs_PlainTime_since(void) {}
void temporal_rs_PlainTime_subtract(void) {}
void temporal_rs_PlainTime_to_ixdtf_string(void) {}
void temporal_rs_PlainTime_try_new(void) {}
void temporal_rs_PlainTime_until(void) {}
void temporal_rs_PlainTime_with(void) {}

// PlainYearMonth
void temporal_rs_PlainYearMonth_add(void) {}
void temporal_rs_PlainYearMonth_calendar(void) {}
void temporal_rs_PlainYearMonth_clone(void) {}
void temporal_rs_PlainYearMonth_compare(void) {}
void temporal_rs_PlainYearMonth_days_in_month(void) {}
void temporal_rs_PlainYearMonth_days_in_year(void) {}
void temporal_rs_PlainYearMonth_destroy(void) {}
void temporal_rs_PlainYearMonth_epoch_ms_for_with_provider(void) {}
void temporal_rs_PlainYearMonth_equals(void) {}
void temporal_rs_PlainYearMonth_era(void) {}
void temporal_rs_PlainYearMonth_era_year(void) {}
void temporal_rs_PlainYearMonth_from_parsed(void) {}
void temporal_rs_PlainYearMonth_from_partial(void) {}
void temporal_rs_PlainYearMonth_in_leap_year(void) {}
void temporal_rs_PlainYearMonth_month(void) {}
void temporal_rs_PlainYearMonth_month_code(void) {}
void temporal_rs_PlainYearMonth_months_in_year(void) {}
void temporal_rs_PlainYearMonth_since(void) {}
void temporal_rs_PlainYearMonth_subtract(void) {}
void temporal_rs_PlainYearMonth_to_ixdtf_string(void) {}
void temporal_rs_PlainYearMonth_to_plain_date(void) {}
void temporal_rs_PlainYearMonth_try_new_with_overflow(void) {}
void temporal_rs_PlainYearMonth_until(void) {}
void temporal_rs_PlainYearMonth_with(void) {}
void temporal_rs_PlainYearMonth_year(void) {}

// Provider / TimeZone
void temporal_rs_Provider_destroy(void) {}
void temporal_rs_Provider_empty(void) {}
void temporal_rs_Provider_new_zoneinfo64(void) {}
void temporal_rs_TimeZone_identifier_with_provider(void) {}
void temporal_rs_TimeZone_try_from_identifier_str_with_provider(void) {}
void temporal_rs_TimeZone_try_from_offset_str(void) {}
void temporal_rs_TimeZone_try_from_str_with_provider(void) {}
void temporal_rs_TimeZone_utc_with_provider(void) {}
void temporal_rs_TimeZone_zero(void) {}

// ZonedDateTime
void temporal_rs_ZonedDateTime_add_with_provider(void) {}
void temporal_rs_ZonedDateTime_calendar(void) {}
void temporal_rs_ZonedDateTime_clone(void) {}
void temporal_rs_ZonedDateTime_compare_instant(void) {}
void temporal_rs_ZonedDateTime_day(void) {}
void temporal_rs_ZonedDateTime_day_of_week(void) {}
void temporal_rs_ZonedDateTime_day_of_year(void) {}
void temporal_rs_ZonedDateTime_days_in_month(void) {}
void temporal_rs_ZonedDateTime_days_in_week(void) {}
void temporal_rs_ZonedDateTime_days_in_year(void) {}
void temporal_rs_ZonedDateTime_destroy(void) {}
void temporal_rs_ZonedDateTime_epoch_milliseconds(void) {}
void temporal_rs_ZonedDateTime_epoch_nanoseconds(void) {}
void temporal_rs_ZonedDateTime_equals_with_provider(void) {}
void temporal_rs_ZonedDateTime_era(void) {}
void temporal_rs_ZonedDateTime_era_year(void) {}
void temporal_rs_ZonedDateTime_from_parsed_with_provider(void) {}
void temporal_rs_ZonedDateTime_from_partial_with_provider(void) {}
void temporal_rs_ZonedDateTime_get_time_zone_transition_with_provider(void) {}
void temporal_rs_ZonedDateTime_hour(void) {}
void temporal_rs_ZonedDateTime_hours_in_day_with_provider(void) {}
void temporal_rs_ZonedDateTime_in_leap_year(void) {}
void temporal_rs_ZonedDateTime_microsecond(void) {}
void temporal_rs_ZonedDateTime_millisecond(void) {}
void temporal_rs_ZonedDateTime_minute(void) {}
void temporal_rs_ZonedDateTime_month(void) {}
void temporal_rs_ZonedDateTime_month_code(void) {}
void temporal_rs_ZonedDateTime_months_in_year(void) {}
void temporal_rs_ZonedDateTime_nanosecond(void) {}
void temporal_rs_ZonedDateTime_offset(void) {}
void temporal_rs_ZonedDateTime_offset_nanoseconds(void) {}
void temporal_rs_ZonedDateTime_round_with_provider(void) {}
void temporal_rs_ZonedDateTime_second(void) {}
void temporal_rs_ZonedDateTime_since_with_provider(void) {}
void temporal_rs_ZonedDateTime_start_of_day_with_provider(void) {}
void temporal_rs_ZonedDateTime_subtract_with_provider(void) {}
void temporal_rs_ZonedDateTime_timezone(void) {}
void temporal_rs_ZonedDateTime_to_instant(void) {}
void temporal_rs_ZonedDateTime_to_ixdtf_string_with_provider(void) {}
void temporal_rs_ZonedDateTime_to_plain_date(void) {}
void temporal_rs_ZonedDateTime_to_plain_datetime(void) {}
void temporal_rs_ZonedDateTime_to_plain_time(void) {}
void temporal_rs_ZonedDateTime_try_new_with_provider(void) {}
void temporal_rs_ZonedDateTime_until_with_provider(void) {}
void temporal_rs_ZonedDateTime_week_of_year(void) {}
void temporal_rs_ZonedDateTime_with_calendar(void) {}
void temporal_rs_ZonedDateTime_with_plain_time_and_provider(void) {}
void temporal_rs_ZonedDateTime_with_timezone_with_provider(void) {}
void temporal_rs_ZonedDateTime_with_with_provider(void) {}
void temporal_rs_ZonedDateTime_year(void) {}
void temporal_rs_ZonedDateTime_year_of_week(void) {}
