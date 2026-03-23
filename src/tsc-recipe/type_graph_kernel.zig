// TSC Type Graph WASM Kernel — Data-Oriented Design
// Full type data stored as flat arrays in WASM linear memory.
// JS writes via Uint32Array views. WASM reads at native speed via TurboFan.
//
// Layout (Structure of Arrays):
//   flags[MAX_TYPES]          — TypeFlags per type
//   objectFlags[MAX_TYPES]    — ObjectFlags per type
//   targetId[MAX_TYPES]       — type reference target (for Reference types)
//   aliasId[MAX_TYPES]        — alias target type ID
//   memberCount[MAX_TYPES]    — number of properties/members
//   memberOffset[MAX_TYPES]   — offset into members array
//   unionCount[MAX_TYPES]     — number of union/intersection members
//   unionOffset[MAX_TYPES]    — offset into unions array
//   members[MAX_MEMBERS]      — (nameHash, typeId) pairs for properties
//   unions[MAX_UNIONS]        — typeId list for union/intersection members
//
// Both JS and WASM access the same arrays via Uint32Array views — zero copy.

const MAX_TYPES: u32 = 65536;
const MAX_MEMBERS: u32 = 524288; // 512K member slots
const MAX_UNIONS: u32 = 262144; // 256K union member slots

// ── Type Data Arrays ──
var type_flags: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;
var type_objectFlags: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;
var type_targetId: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;
var type_memberCount: [MAX_TYPES]u16 = [_]u16{0} ** MAX_TYPES;
var type_memberOffset: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;
var type_unionCount: [MAX_TYPES]u16 = [_]u16{0} ** MAX_TYPES;
var type_unionOffset: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;

// Member table: pairs of (nameHash, typeId) — for property comparison
var member_nameHash: [MAX_MEMBERS]u32 = [_]u32{0} ** MAX_MEMBERS;
var member_typeId: [MAX_MEMBERS]u32 = [_]u32{0} ** MAX_MEMBERS;
var member_flags: [MAX_MEMBERS]u16 = [_]u16{0} ** MAX_MEMBERS; // optional, readonly etc.

// Union/intersection member list
var union_members: [MAX_UNIONS]u32 = [_]u32{0} ** MAX_UNIONS;

var type_count: u32 = 0;
var member_count: u32 = 0;
var union_count: u32 = 0;

// ── TypeFlags constants ──
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

// ── Relation Cache ──
const CACHE_SIZE: u32 = 32768;
var cache_key1: [CACHE_SIZE]u32 = [_]u32{0} ** CACHE_SIZE;
var cache_key2: [CACHE_SIZE]u32 = [_]u32{0} ** CACHE_SIZE;
var cache_val: [CACHE_SIZE]u32 = [_]u32{0} ** CACHE_SIZE;
var cache_used: [CACHE_SIZE]u8 = [_]u8{0} ** CACHE_SIZE;

// ── Exports: Array Pointers ──
export fn getFlagsPtr() u32 { return @intFromPtr(&type_flags); }
export fn getObjectFlagsPtr() u32 { return @intFromPtr(&type_objectFlags); }
export fn getTargetIdPtr() u32 { return @intFromPtr(&type_targetId); }
export fn getMemberCountPtr() u32 { return @intFromPtr(&type_memberCount); }
export fn getMemberOffsetPtr() u32 { return @intFromPtr(&type_memberOffset); }
export fn getUnionCountPtr() u32 { return @intFromPtr(&type_unionCount); }
export fn getUnionOffsetPtr() u32 { return @intFromPtr(&type_unionOffset); }
export fn getMemberNameHashPtr() u32 { return @intFromPtr(&member_nameHash); }
export fn getMemberTypeIdPtr() u32 { return @intFromPtr(&member_typeId); }
export fn getMemberFlagsPtr() u32 { return @intFromPtr(&member_flags); }
export fn getUnionMembersPtr() u32 { return @intFromPtr(&union_members); }

// ── Registration ──

export fn registerType(id: u32, flags: u32, obj_flags: u32) void {
    if (id >= MAX_TYPES) return;
    type_flags[id] = flags;
    type_objectFlags[id] = obj_flags;
    if (id >= type_count) type_count = id + 1;
}

export fn setTypeTarget(id: u32, target_id: u32) void {
    if (id >= MAX_TYPES) return;
    type_targetId[id] = target_id;
}

export fn addMember(type_id: u32, name_hash: u32, member_type_id: u32, mflags: u32) void {
    if (type_id >= MAX_TYPES or member_count >= MAX_MEMBERS) return;
    if (type_memberCount[type_id] == 0) {
        type_memberOffset[type_id] = member_count;
    }
    member_nameHash[member_count] = name_hash;
    member_typeId[member_count] = member_type_id;
    member_flags[member_count] = @intCast(mflags & 0xFFFF);
    member_count += 1;
    type_memberCount[type_id] += 1;
}

export fn addUnionMember(type_id: u32, member_id: u32) void {
    if (type_id >= MAX_TYPES or union_count >= MAX_UNIONS) return;
    if (type_unionCount[type_id] == 0) {
        type_unionOffset[type_id] = union_count;
    }
    union_members[union_count] = member_id;
    union_count += 1;
    type_unionCount[type_id] += 1;
}

// ── Type Relation Check ──

export fn isTypeRelated(src_id: u32, tgt_id: u32, rel: u32, strict_null: u32) u32 {
    if (src_id == tgt_id) return 1;
    if (src_id >= MAX_TYPES or tgt_id >= MAX_TYPES) return 0;

    const s = type_flags[src_id];
    const t = type_flags[tgt_id];
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

    // ── Relation cache ──
    if (s & Object != 0 and t & Object != 0) {
        const cached = cacheGet(src_id, tgt_id, rel);
        if (cached != 255) {
            return if (cached & 1 != 0) @as(u32, 1) else @as(u32, 2);
        }
    }

    // Structural comparison and union handling — data stored in WASM for future use.
    // Currently disabled for correctness (member type resolution is incomplete).
    // The member/union data is populated but only used for cache lookups.

    return 0; // Inconclusive — fall through to JS
}

// ── Cache ──
fn cacheHash(a: u32, b: u32) u32 {
    var h: u32 = 2166136261;
    h ^= a; h *%= 16777619;
    h ^= b; h *%= 16777619;
    return h & (CACHE_SIZE - 1);
}

fn cacheGet(src: u32, tgt: u32, rel: u32) u8 {
    const bucket = cacheHash(src, tgt);
    var i: u32 = 0;
    while (i < 4) : (i += 1) {
        const slot = (bucket + i) & (CACHE_SIZE - 1);
        if (cache_used[slot] == 0) return 255;
        if (cache_key1[slot] == src and cache_key2[slot] == tgt and (cache_val[slot] >> 8) == rel) {
            return @intCast(cache_val[slot] & 0xFF);
        }
    }
    return 255;
}

export fn cacheSet(src: u32, tgt: u32, rel: u32, value: u32) void {
    const bucket = cacheHash(src, tgt);
    var i: u32 = 0;
    while (i < 4) : (i += 1) {
        const slot = (bucket + i) & (CACHE_SIZE - 1);
        if (cache_used[slot] == 0 or (cache_key1[slot] == src and cache_key2[slot] == tgt and (cache_val[slot] >> 8) == rel)) {
            cache_key1[slot] = src;
            cache_key2[slot] = tgt;
            cache_val[slot] = (rel << 8) | (value & 0xFF);
            cache_used[slot] = 1;
            return;
        }
    }
    const slot = bucket;
    cache_key1[slot] = src;
    cache_key2[slot] = tgt;
    cache_val[slot] = (rel << 8) | (value & 0xFF);
    cache_used[slot] = 1;
}

export fn reset() void {
    type_count = 0;
    member_count = 0;
    union_count = 0;
    @memset(&type_flags, 0);
    @memset(&type_objectFlags, 0);
    @memset(&type_targetId, 0);
    @memset(&type_memberCount, 0);
    @memset(&type_unionCount, 0);
    @memset(&cache_used, 0);
}

export fn getStats(out_types: *u32, out_members: *u32, out_unions: *u32) void {
    out_types.* = type_count;
    out_members.* = member_count;
    out_unions.* = union_count;
}
