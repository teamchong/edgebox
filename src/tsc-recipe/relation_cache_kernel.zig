// WASM Kernel: Type Relation Cache
// Replaces TSC's Map<string, RelationComparisonResult> with flat integer hash.
// Eliminates: string key allocation, string hashing, ordered Map overhead.
// Key: hash(sourceId, targetId, intersectionState) → Uint8Array slot.

const CACHE_SIZE: u32 = 1 << 20; // 1M entries
const CACHE_MASK: u32 = CACHE_SIZE - 1;

var cache_keys: [CACHE_SIZE]u64 = [_]u64{0} ** CACHE_SIZE;
var cache_vals: [CACHE_SIZE]u8 = [_]u8{0} ** CACHE_SIZE;
var hits: u32 = 0;
var misses: u32 = 0;
var stores: u32 = 0;

fn makeKey(src: u32, tgt: u32, state: u32) u64 {
    return (@as(u64, src) << 32) | (@as(u64, tgt) << 2) | @as(u64, state & 3);
}

fn hashKey(key: u64) u32 {
    var h = key;
    h ^= h >> 17;
    h *%= 0x9E3779B97F4A7C15; // golden ratio hash
    h ^= h >> 31;
    return @intCast(h & CACHE_MASK);
}

// Get cached relation result. Returns: 0=miss, 1+=cached value.
export fn cacheGet(src_id: u32, tgt_id: u32, intersection_state: u32) u32 {
    const key = makeKey(src_id, tgt_id, intersection_state);
    const slot = hashKey(key);
    if (cache_keys[slot] == key and cache_vals[slot] != 0) {
        hits += 1;
        return cache_vals[slot];
    }
    misses += 1;
    return 0;
}

// Store relation result.
export fn cacheSet(src_id: u32, tgt_id: u32, intersection_state: u32, result: u32) void {
    const key = makeKey(src_id, tgt_id, intersection_state);
    const slot = hashKey(key);
    cache_keys[slot] = key;
    cache_vals[slot] = @intCast(result & 0xFF);
    stores += 1;
}

export fn cacheStats() u64 {
    return (@as(u64, hits) << 32) | @as(u64, misses);
}

export fn cacheReset() void {
    hits = 0;
    misses = 0;
    stores = 0;
    @memset(&cache_vals, 0);
    @memset(&cache_keys, 0);
}
