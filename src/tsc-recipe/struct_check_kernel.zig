// WASM SIMD kernel: structural type comparison
// Compares member name arrays using SIMD — 4 u32 comparisons per cycle.
// This replaces TSC's O(n*m) JS property lookup with O(n*m/4) SIMD scan.

const std = @import("std");

// Type data stored in WASM linear memory (flat arrays)
// Layout: flags[MAX_TYPES], member_offset[MAX_TYPES], member_count[MAX_TYPES],
//         member_names[MAX_MEMBERS], member_types[MAX_MEMBERS]
const MAX_TYPES: u32 = 65536;
const MAX_MEMBERS: u32 = 524288;

var flags: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;
var member_offset: [MAX_TYPES]u32 = [_]u32{0} ** MAX_TYPES;
var member_count: [MAX_TYPES]u16 = [_]u16{0} ** MAX_TYPES;
var member_names: [MAX_MEMBERS]u32 = [_]u32{0} ** MAX_MEMBERS;
var member_types: [MAX_MEMBERS]u32 = [_]u32{0} ** MAX_MEMBERS;
var type_count: u32 = 0;
var total_members: u32 = 0;

export fn registerType(id: u32, f: u32) void {
    if (id >= MAX_TYPES) return;
    flags[id] = f;
    if (id >= type_count) type_count = id + 1;
}

export fn registerMember(type_id: u32, name_id: u32, type_id_member: u32) void {
    if (type_id >= MAX_TYPES or total_members >= MAX_MEMBERS) return;
    if (member_count[type_id] == 0) member_offset[type_id] = total_members;
    const idx = total_members;
    member_names[idx] = name_id;
    member_types[idx] = type_id_member;
    total_members += 1;
    member_count[type_id] += 1;
}

// SIMD structural check: does source satisfy target's member requirements?
// For each target member name, scan source members for a match.
// Returns: 1 = structurally compatible, 0 = not compatible or unknown.
export fn structuralCheck(source_id: u32, target_id: u32) u32 {
    if (source_id == target_id) return 1;
    if (source_id >= type_count or target_id >= type_count) return 0;

    const src_off = member_offset[source_id];
    const src_cnt = member_count[source_id];
    const tgt_off = member_offset[target_id];
    const tgt_cnt = member_count[target_id];

    if (tgt_cnt == 0 and src_cnt == 0) return 0; // can't determine
    if (tgt_cnt == 0) return 0; // no target members → unknown
    if (src_cnt == 0) return 0; // no source members → can't satisfy

    // For each target member, find matching source member
    var ti: u32 = 0;
    while (ti < tgt_cnt) : (ti += 1) {
        const tgt_name = member_names[tgt_off + ti];
        const tgt_type = member_types[tgt_off + ti];
        var found = false;

        // SIMD scan: compare 4 source member names at once
        var si: u32 = 0;
        const target_vec: @Vector(4, u32) = @splat(tgt_name);
        while (si + 4 <= src_cnt) : (si += 4) {
            const src_vec: @Vector(4, u32) = .{
                member_names[src_off + si],
                member_names[src_off + si + 1],
                member_names[src_off + si + 2],
                member_names[src_off + si + 3],
            };
            const matches = src_vec == target_vec;
            if (@reduce(.Or, matches)) {
                // Found a match — check which one and verify type
                var j: u32 = 0;
                while (j < 4) : (j += 1) {
                    if (member_names[src_off + si + j] == tgt_name) {
                        if (member_types[src_off + si + j] == tgt_type) {
                            found = true;
                            break;
                        }
                    }
                }
                if (found) break;
            }
        }
        // Scalar remainder
        if (!found) {
            while (si < src_cnt) : (si += 1) {
                if (member_names[src_off + si] == tgt_name and member_types[src_off + si] == tgt_type) {
                    found = true;
                    break;
                }
            }
        }
        if (!found) return 0;
    }
    return 1;
}

export fn reset() void {
    type_count = 0;
    total_members = 0;
}
