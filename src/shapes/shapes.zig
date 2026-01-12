//! Shape Optimization Module
//!
//! Provides SIMD-only, comptime-analyzable shape detection and columnar storage.
//! All code paths are SIMD - no scalar fallback.
//! When not used, Zig's DCE removes the entire module.
//!
//! Usage:
//!   const shapes = @import("shapes/shapes.zig");
//!
//!   // Comptime shape generation (DCE removes if unused)
//!   const AstStorage = shapes.static_analyzer.AstNodeStorage;
//!
//!   // Runtime columnar storage
//!   var storage = try shapes.columnar.DynamicColumnarStorage.init(...);

pub const static_analyzer = @import("static_analyzer.zig");
pub const columnar = @import("columnar_storage.zig");

// Re-export common types
pub const ColumnarStorage = static_analyzer.ColumnarStorage;
pub const ShapeField = static_analyzer.ShapeField;
pub const PropertyAccess = static_analyzer.PropertyAccess;
pub const AccessType = static_analyzer.AccessType;
pub const ComptimeAnalysis = static_analyzer.ComptimeAnalysis;

pub const DynamicColumnarStorage = columnar.DynamicColumnarStorage;
pub const FilterCondition = columnar.FilterCondition;

// Re-export SIMD types
pub const Vec8i32 = static_analyzer.Vec8i32;
pub const Vec8u32 = static_analyzer.Vec8u32;
pub const Vec8i64 = static_analyzer.Vec8i64;
pub const Vec8u64 = static_analyzer.Vec8u64;
pub const Vec8f64 = static_analyzer.Vec8f64;

// Re-export SIMD utilities
pub const simdSum8i32 = static_analyzer.simdSum8i32;
pub const simdSum8i64 = static_analyzer.simdSum8i64;
pub const simdMin8i32 = static_analyzer.simdMin8i32;
pub const simdMax8i32 = static_analyzer.simdMax8i32;
pub const simdCompareEq8i64 = static_analyzer.simdCompareEq8i64;
pub const simdGather8i64 = static_analyzer.simdGather8i64;
pub const simdScatter8i64 = static_analyzer.simdScatter8i64;

// Pre-defined shapes (DCE removes if unused)
pub const AstNodeFields = static_analyzer.AstNodeFields;
pub const AstNodeStorage = static_analyzer.AstNodeStorage;
pub const Vector3Fields = static_analyzer.Vector3Fields;
pub const Vector3Storage = static_analyzer.Vector3Storage;
pub const KeyValueFields = static_analyzer.KeyValueFields;
pub const KeyValueStorage = static_analyzer.KeyValueStorage;

test {
    _ = static_analyzer;
    _ = columnar;
}
