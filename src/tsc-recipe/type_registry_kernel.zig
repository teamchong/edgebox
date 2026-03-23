// TSC Type Registry WASM Kernel
// Stores type flags in WASM linear memory as flat arrays.
// JS writes via Uint32Array view. WASM reads same memory. Zero copy.
//
// Memory: global arrays in .bss → WASM linear memory.
// JS accesses via: new Uint32Array(wasm.memory.buffer, flagsPtr, MAX_TYPES)

const MAX_TYPES: u32 = 65536; // 64K types (256KB for flags)
const CACHE_SIZE: u32 = 32768; // 32K cache slots

// ── Type Flags Array ──
// Indexed by type.id — stores TSC TypeFlags for each type.
var type_flags: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;
var type_count: u32 = 0;

// ── Relation Cache ──
// Integer hash map: (sourceId, targetId) → result
// 3 entries per slot: [source_id, target_id, rel_and_result]
var cache_src: [CACHE_SIZE]u32 = [_]u32{0} ** CACHE_SIZE;
var cache_tgt: [CACHE_SIZE]u32 = [_]u32{0} ** CACHE_SIZE;
var cache_val: [CACHE_SIZE]u32 = [_]u32{0} ** CACHE_SIZE; // (rel << 8) | result
var cache_used: [CACHE_SIZE]u8 = [_]u8{0} ** CACHE_SIZE;

// TSC TypeFlags constants
const Any: u32 = 1;
const Unknown: u32 = 2;
const String: u32 = 4;
const Number: u32 = 8;
const Boolean: u32 = 16;
const Enum: u32 = 32;
const BigInt: u32 = 64;
const StringLiteral: u32 = 128;
const NumberLiteral: u32 = 256;
const BooleanLiteral: u32 = 512;
const EnumLiteral: u32 = 1024;
const BigIntLiteral: u32 = 2048;
const ESSymbol: u32 = 4096;
const Void: u32 = 16384;
const Undefined: u32 = 32768;
const Null: u32 = 65536;
const Never: u32 = 131072;
const Object: u32 = 524288;
const Union: u32 = 1048576;
const Intersection: u32 = 2097152;
const NonPrimitive: u32 = 67108864;
const StringLike: u32 = 402653316;
const NumberLike: u32 = 296;
const BigIntLike: u32 = 2112;
const BooleanLike: u32 = 528;
const ESSymbolLike: u32 = 12288;

const REL_ASSIGNABLE: u32 = 0;
const REL_COMPARABLE: u32 = 1;
const REL_STRICT_SUBTYPE: u32 = 2;

/// Export pointer to flags array so JS can create a Uint32Array view.
export fn getFlagsPtr() u32 {
    return @intFromPtr(&type_flags);
}

/// Export pointer to cache arrays for JS access.
export fn getCacheSrcPtr() u32 { return @intFromPtr(&cache_src); }
export fn getCacheTgtPtr() u32 { return @intFromPtr(&cache_tgt); }
export fn getCacheValPtr() u32 { return @intFromPtr(&cache_val); }

/// Register a type. JS calls this OR writes directly to the Uint32Array view.
export fn registerType(id: u32, flags: u32) void {
    if (id >= MAX_TYPES) return;
    type_flags[id] = flags;
    if (id >= type_count) type_count = id + 1;
}

/// Full isTypeRelatedTo — reads flags from array, checks cache.
/// Returns: 1 = related, 2 = not related, 0 = inconclusive (need JS)
export fn isTypeRelated(source_id: u32, target_id: u32, rel: u32, strict_null: u32) u32 {
    if (source_id == target_id) return 1;

    const s = if (source_id < MAX_TYPES) type_flags[source_id] else return 0;
    const t = if (target_id < MAX_TYPES) type_flags[target_id] else return 0;
    if (s == 0 or t == 0) return 0;

    // ── Definite TRUE ──
    if (t & Any != 0 or s & Never != 0) return 1;
    if (t & Unknown != 0 and !(rel == REL_STRICT_SUBTYPE and s & Any != 0)) return 1;
    if (t & Never != 0) return 2;

    if (s & StringLike != 0 and t & String != 0) return 1;
    if (s & NumberLike != 0 and t & Number != 0) return 1;
    if (s & BigIntLike != 0 and t & BigInt != 0) return 1;
    if (s & BooleanLike != 0 and t & Boolean != 0) return 1;
    if (s & ESSymbolLike != 0 and t & ESSymbol != 0) return 1;

    if (s & Undefined != 0) {
        if (strict_null == 0 or t & (Undefined | Void) != 0) return 1;
    }
    if (s & Null != 0) {
        if (strict_null == 0 or t & Null != 0) return 1;
    }
    if (s & Object != 0 and t & NonPrimitive != 0) return 1;

    if (rel == REL_ASSIGNABLE or rel == REL_COMPARABLE) {
        if (s & Any != 0) return 1;
        if (s & Number != 0 and t & Enum != 0) return 1;
    }

    // ── Definite FALSE ──
    if (s & Object != 0 and t & Object == 0 and t & NonPrimitive == 0 and
        t & Any == 0 and t & Unknown == 0 and t & (Union | Intersection) == 0)
    {
        return 2;
    }
    if (s & (String | Number | Boolean | BigInt | ESSymbol | Void | Undefined | Null | Never) != 0 and
        t & (String | Number | Boolean | BigInt | ESSymbol | Void | Undefined | Null | Never) != 0)
    {
        if (rel != REL_COMPARABLE) return 2;
    }
    if (s & StringLiteral != 0 and t & (String | Any | Unknown | StringLiteral) == 0) {
        if (t & (Union | Intersection) == 0) return 2;
    }

    // ── Relation cache ──
    if (s & Object != 0 and t & Object != 0) {
        const cached = cacheGet(source_id, target_id, rel);
        if (cached != 255) {
            return if (cached & 1 != 0) @as(u32, 1) else @as(u32, 2);
        }
    }

    return 0;
}

// ── Cache (FNV-1a hash, open addressing) ──

fn cacheHash(src: u32, tgt: u32) u32 {
    var h: u32 = 2166136261;
    h ^= src;
    h *%= 16777619;
    h ^= tgt;
    h *%= 16777619;
    return h & (CACHE_SIZE - 1);
}

fn cacheGet(src: u32, tgt: u32, rel: u32) u8 {
    const bucket = cacheHash(src, tgt);
    // Linear probe (4 slots)
    var i: u32 = 0;
    while (i < 4) : (i += 1) {
        const slot = (bucket + i) & (CACHE_SIZE - 1);
        if (cache_used[slot] == 0) return 255;
        if (cache_src[slot] == src and cache_tgt[slot] == tgt and (cache_val[slot] >> 8) == rel) {
            return @intCast(cache_val[slot] & 0xFF);
        }
    }
    return 255;
}

export fn cacheSet(src_id: u32, tgt_id: u32, rel: u32, value: u32) void {
    const bucket = cacheHash(src_id, tgt_id);
    var i: u32 = 0;
    while (i < 4) : (i += 1) {
        const slot = (bucket + i) & (CACHE_SIZE - 1);
        if (cache_used[slot] == 0 or (cache_src[slot] == src_id and cache_tgt[slot] == tgt_id and (cache_val[slot] >> 8) == rel)) {
            cache_src[slot] = src_id;
            cache_tgt[slot] = tgt_id;
            cache_val[slot] = (rel << 8) | (value & 0xFF);
            cache_used[slot] = 1;
            return;
        }
    }
    // All 4 slots full — evict first
    const slot = bucket;
    cache_src[slot] = src_id;
    cache_tgt[slot] = tgt_id;
    cache_val[slot] = (rel << 8) | (value & 0xFF);
    cache_used[slot] = 1;
}

export fn reset() void {
    type_count = 0;
    @memset(&type_flags, 0);
    @memset(&cache_used, 0);
}

export fn getTypeCount() u32 {
    return type_count;
}
