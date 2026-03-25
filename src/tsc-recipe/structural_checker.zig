// Structural Type Checker — compiled to WASM via `zig build-exe -target wasm32-freestanding`
//
// Replaces TSC's propertiesRelatedTo inner loop with native WASM.
// Stores type property tables in linear memory. JS populates during createType,
// WASM compares during structuredTypeRelatedTo.
//
// Memory layout per type (max 65536 types):
//   propertyTable[typeId] → offset into propData
//   propData[offset] = { count: u16, entries: [{nameHash: u32, typeId: u16, flags: u16}] }
//   Each entry = 8 bytes. Max 64 properties per type.
//
// Exported functions:
//   initType(typeId, propCount) → slot offset
//   addProp(typeId, slotIdx, nameHash, propTypeId, propFlags)
//   checkProperties(srcId, tgtId) → 1=compatible, 0=incompatible, -1=unknown
//   setFlags(typeId, flags)
//   getFlags(typeId) → flags

const std = @import("std");

// Type capacity
const MAX_TYPES: u32 = 65536;
const MAX_PROPS_PER_TYPE: u32 = 64;
const PROP_ENTRY_SIZE: u32 = 8; // nameHash(4) + typeId(2) + flags(2)

// Property entry layout
const PropEntry = extern struct {
    name_hash: u32,
    type_id: u16,
    flags: u16,
};

// Per-type header: property count + padding
const TypeHeader = extern struct {
    prop_count: u16,
    _pad: u16 = 0,
    flags: u32, // TypeFlags from TSC
};

// Fixed memory layout:
// [0 .. MAX_TYPES * 8)                = TypeHeader array (8 bytes each)
// [MAX_TYPES * 8 .. MAX_TYPES * 8 + MAX_TYPES * MAX_PROPS * 8) = PropEntry arrays
const HEADER_SIZE: u32 = @sizeOf(TypeHeader); // 8
const HEADERS_END: u32 = MAX_TYPES * HEADER_SIZE; // 524288
const PROP_AREA_START: u32 = HEADERS_END;

// Memory: headers + property data
var memory: [HEADERS_END + MAX_TYPES * MAX_PROPS_PER_TYPE * PROP_ENTRY_SIZE]u8 = undefined;

fn getHeader(type_id: u32) *TypeHeader {
    if (type_id >= MAX_TYPES) return @ptrCast(@alignCast(&memory[0]));
    const off = type_id * HEADER_SIZE;
    return @ptrCast(@alignCast(&memory[off]));
}

fn getPropEntry(type_id: u32, idx: u32) *PropEntry {
    if (type_id >= MAX_TYPES or idx >= MAX_PROPS_PER_TYPE) {
        return @ptrCast(@alignCast(&memory[PROP_AREA_START]));
    }
    const off = PROP_AREA_START + (type_id * MAX_PROPS_PER_TYPE + idx) * PROP_ENTRY_SIZE;
    return @ptrCast(@alignCast(&memory[off]));
}

// ── Exports ──

export fn initType(type_id: u32, prop_count: u32) void {
    if (type_id >= MAX_TYPES) return;
    const h = getHeader(type_id);
    h.prop_count = @intCast(@min(prop_count, MAX_PROPS_PER_TYPE));
}

export fn addProp(type_id: u32, slot_idx: u32, name_hash: u32, prop_type_id: u32, prop_flags: u32) void {
    if (type_id >= MAX_TYPES or slot_idx >= MAX_PROPS_PER_TYPE) return;
    const e = getPropEntry(type_id, slot_idx);
    e.name_hash = name_hash;
    e.type_id = @intCast(prop_type_id & 0xFFFF);
    e.flags = @intCast(prop_flags & 0xFFFF);
}

export fn setFlags(type_id: u32, flags: u32) void {
    if (type_id >= MAX_TYPES) return;
    getHeader(type_id).flags = flags;
}

export fn getFlags(type_id: u32) u32 {
    if (type_id >= MAX_TYPES) return 0;
    return getHeader(type_id).flags;
}

/// Check if source's properties are compatible with target's properties.
/// For each property in target, checks if source has a property with the same nameHash.
/// If source is missing a required (non-optional) target property → incompatible.
/// Returns: 1=all target props found in source, 0=missing required prop, -1=unknown/complex
export fn checkProperties(src_id: u32, tgt_id: u32) i32 {
    if (src_id >= MAX_TYPES or tgt_id >= MAX_TYPES) return -1;

    const src_h = getHeader(src_id);
    const tgt_h = getHeader(tgt_id);

    // No properties registered → can't decide
    if (tgt_h.prop_count == 0) return -1;
    if (src_h.prop_count == 0 and tgt_h.prop_count > 0) return -1;

    const tgt_count = tgt_h.prop_count;
    const src_count = src_h.prop_count;

    // For each target property, find matching source property by nameHash
    var i: u32 = 0;
    while (i < tgt_count) : (i += 1) {
        const tgt_prop = getPropEntry(tgt_id, i);
        const tgt_name = tgt_prop.name_hash;

        // Optional property (flag bit 0) — skip if not in source
        const is_optional = (tgt_prop.flags & 1) != 0;

        var found = false;
        var j: u32 = 0;
        while (j < src_count) : (j += 1) {
            const src_prop = getPropEntry(src_id, j);
            if (src_prop.name_hash == tgt_name) {
                found = true;
                // Property found — check type compatibility via flags
                // If both property types are registered, do a quick flag check
                const src_prop_type = src_prop.type_id;
                const tgt_prop_type = tgt_prop.type_id;
                if (src_prop_type > 0 and src_prop_type < MAX_TYPES and
                    tgt_prop_type > 0 and tgt_prop_type < MAX_TYPES)
                {
                    const sf = getHeader(src_prop_type).flags;
                    const tf = getHeader(tgt_prop_type).flags;
                    // Same type → compatible
                    if (src_prop_type == tgt_prop_type) break;
                    // Quick flag check (same as checkFlags):
                    // StringLike→String, NumberLike→Number, etc.
                    if (quickFlagCheck(sf, tf)) break;
                    // If both are structured types, we can't decide here
                    if ((sf & 469499904) != 0 or (tf & 469499904) != 0) return -1;
                    // Incompatible primitives
                    if ((sf & 249860) != 0 and (tf & 249860) != 0) {
                        if ((sf & tf & 249860) == 0) return 0;
                    }
                }
                break;
            }
        }
        if (!found and !is_optional) return 0; // Required property missing
    }
    return 1; // All target properties found in source
}

/// Quick flag compatibility check (subset of isSimpleTypeRelatedTo)
fn quickFlagCheck(s: u32, t: u32) bool {
    if ((t & 1) != 0) return true; // target Any
    if ((s & 131072) != 0) return true; // source Never
    if ((t & 2) != 0) return true; // target Unknown
    if ((s & 402653316) != 0 and (t & 4) != 0) return true; // StringLike→String
    if ((s & 296) != 0 and (t & 8) != 0) return true; // NumberLike→Number
    if ((s & 2112) != 0 and (t & 64) != 0) return true; // BigIntLike→BigInt
    if ((s & 528) != 0 and (t & 16) != 0) return true; // BooleanLike→Boolean
    if ((s & 32768) != 0 and (t & 49152) != 0) return true; // Undefined→Undefined|Void
    if ((s & 65536) != 0 and (t & 65536) != 0) return true; // Null→Null
    return false;
}

/// Hash a property name (FNV-1a 32-bit)
export fn hashName(ptr: [*]const u8, len: u32) u32 {
    var hash: u32 = 2166136261;
    var i: u32 = 0;
    while (i < len) : (i += 1) {
        hash ^= ptr[i];
        hash *%= 16777619;
    }
    return hash;
}

/// Check if source type's properties are a superset of target's (by name only)
/// Faster than checkProperties — only checks name presence, not type compatibility
export fn hasAllProperties(src_id: u32, tgt_id: u32) i32 {
    if (src_id >= MAX_TYPES or tgt_id >= MAX_TYPES) return -1;
    const src_h = getHeader(src_id);
    const tgt_h = getHeader(tgt_id);
    if (tgt_h.prop_count == 0) return -1;
    if (src_h.prop_count == 0) return -1;

    var i: u32 = 0;
    while (i < tgt_h.prop_count) : (i += 1) {
        const tgt_name = getPropEntry(tgt_id, i).name_hash;
        const is_optional = (getPropEntry(tgt_id, i).flags & 1) != 0;
        var found = false;
        var j: u32 = 0;
        while (j < src_h.prop_count) : (j += 1) {
            if (getPropEntry(src_id, j).name_hash == tgt_name) { found = true; break; }
        }
        if (!found and !is_optional) return 0;
    }
    return 1;
}
