// v8_tsc_transforms.zig — shared TSC source transforms
//
// Applied to _tsc.js at runtime (v8_runner.zig) and to typescript.js
// at snapshot-gen time (v8_snapshot_gen.zig). Both use the SAME transforms
// to ensure snapshot fast-path produces identical results.

const std = @import("std");

const Transform = struct {
    needle: []const u8,
    replacement: []const u8,
};

/// All TSC source transforms. Order matters — first match wins.
pub const transforms = [_]Transform{
    // T1: createType → write flags to SAB-backed __pc_typeFlags + trigger async build
    .{ .needle = "typeCount++;\n    result.id = typeCount;", .replacement = "typeCount++;\n    result.id = typeCount;\n    if(typeof __pc_typeFlags!=='undefined'&&typeCount<262144){__pc_typeFlags[typeCount]=result.flags;if(result.objectFlags)__pc_objectFlags[typeCount]=result.objectFlags;if(typeCount===5000&&typeof __edgebox_trigger_build==='function')__edgebox_trigger_build(typeCount);}" },
    // T2: isTypeRelatedTo → direct-mapped cache + inline flag fast-path.
    // Flag fast-path: Any/Never/primitive widening (safe, no cache needed).
    // Direct-mapped cache: O(1) lookup via __rc_keys/vals for structural results.
    // Skip isSimpleTypeRelatedTo when source can't match remaining cases.
    .{ .needle = "function isTypeRelatedTo(source, target, relation) {\n    if (isFreshLiteralType(source)) {\n      source = source.regularType;\n    }\n    if (isFreshLiteralType(target)) {\n      target = target.regularType;\n    }\n    if (source === target) {\n      return true;\n    }", .replacement = "function isTypeRelatedTo(source, target, relation) {\n    if (isFreshLiteralType(source)) {\n      source = source.regularType;\n    }\n    if (isFreshLiteralType(target)) {\n      target = target.regularType;\n    }\n    if (source === target) {\n      return true;\n    }\n    var __tf=target.flags;if(__tf&1)return true;var __sf=source.flags;if(__sf&131072)return true;if(__sf&402653316&&__tf&4||__sf&296&&__tf&8||__sf&2112&&__tf&64||__sf&528&&__tf&16||__sf&12288&&__tf&4096)return true;\n    if(typeof __rc_keys!=='undefined'&&relation.__rid){var __rk=((source.id*131+target.id)*5+relation.__rid)|0,__ri=__rk&131071,__pk=(__rk&0x7FFFFFFF)|0;if(__rc_keys[__ri]===__pk){var __rv=__rc_vals[__ri];if(__rv)return __rv>0;}}\n    if(__tf&2&&!(relation===strictSubtypeRelation&&__sf&1))return true;" },
    // T2b: isTypeRelatedTo return paths — write result to direct-mapped cache.
    // Only cache checkTypeRelatedTo results (expensive structural comparison).
    // The relation.get cache inside checkTypeRelatedTo is relation-specific,
    // but our cache is relation-agnostic (may overcache). Safe because:
    // - Same (source, target) pair returns same result regardless of relation
    //   for the non-identity relations (subtype/assignable/comparable).
    //   Actually this is NOT safe — different relations give different results.
    //   So we include relation.__rid in the cache key.
    .{ .needle = "if (source.flags & 469499904 /* StructuredOrInstantiable */ || target.flags & 469499904 /* StructuredOrInstantiable */) {\n      return checkTypeRelatedTo(\n        source,\n        target,\n        relation,\n        /*errorNode*/\n        void 0\n      );\n    }\n    return false;\n  }", .replacement = "if (source.flags & 469499904 || target.flags & 469499904) {\n      var __cr=checkTypeRelatedTo(source,target,relation,void 0);\n      if(typeof __rc_keys!=='undefined'&&relation.__rid){var __wk=((source.id*131+target.id)*5+relation.__rid)|0,__wi=__wk&131071;__rc_keys[__wi]=(__wk&0x7FFFFFFF)|0;__rc_vals[__wi]=__cr?1:-1;}return __cr;\n    }\n    return false;\n  }" },
    // T3: getRelationKey → packed Smi when IDs fit (< 32768) and no intersectionState,
    // OR packed double when intersectionState > 0 (no string allocation)
    .{ .needle = "isTypeReferenceWithGenericArguments(source) && isTypeReferenceWithGenericArguments(target) ? getGenericTypeReferenceRelationKey(source, target, postFix, ignoreConstraints) : `${source.id},${target.id}${postFix}`", .replacement = "isTypeReferenceWithGenericArguments(source) && isTypeReferenceWithGenericArguments(target) ? getGenericTypeReferenceRelationKey(source, target, postFix, ignoreConstraints) : (!postFix&&source.id<32768&&target.id<32768) ? source.id * 32768 + target.id + 1 : `${source.id},${target.id}${postFix}`" },
    // T4: JSDoc skip
    .{ .needle = "jsDocParsingMode = 0", .replacement = "jsDocParsingMode = 1" },
    // T5: typeof guard for packed integer key
    .{ .needle = "id.startsWith(\"*\")", .replacement = "(typeof id === \"string\" && id.startsWith(\"*\"))" },
    // T-SOA1: store property count in upper 16 bits of __pc_objectFlags after member resolution
    .{ .needle = "if (members !== emptySymbols) resolved.properties = getNamedMembers(members);\n    return resolved;", .replacement = "if (members !== emptySymbols) resolved.properties = getNamedMembers(members);\n    if(typeof __pc_objectFlags!=='undefined'&&resolved.id>0&&resolved.id<131072)__pc_objectFlags[resolved.id]=(__pc_objectFlags[resolved.id]&0xFFFF)|(resolved.properties.length<<16);\n    return resolved;" },
    // T-SOA2: fast reject in propertiesIdenticalTo using SOA property counts
    .{ .needle = "const sourceProperties = excludeProperties(getPropertiesOfObjectType(source2), excludedProperties);\n      const targetProperties = excludeProperties(getPropertiesOfObjectType(target2), excludedProperties);\n      if (sourceProperties.length !== targetProperties.length) {", .replacement = "if(typeof __pc_objectFlags!=='undefined'&&!excludedProperties&&source2.id>0&&source2.id<131072&&target2.id>0&&target2.id<131072){var __sc=__pc_objectFlags[source2.id]>>>16,__tc=__pc_objectFlags[target2.id]>>>16;if(__sc&&__tc&&__sc!==__tc)return 0;}\n      const sourceProperties = excludeProperties(getPropertiesOfObjectType(source2), excludedProperties);\n      const targetProperties = excludeProperties(getPropertiesOfObjectType(target2), excludedProperties);\n      if (sourceProperties.length !== targetProperties.length) {" },
    // T-NODE: SOA write for node.kind — DISABLED: populated but never read.
    // T-SYMID: SOA write for symbol.flags — DISABLED: populated but never read.
    // T6: getFlowCacheKey Identifier → packed integer
    .{ .needle = "return symbol !== unknownSymbol ? `${flowContainer ? getNodeId(flowContainer) : \"-1\"}|${getTypeId(declaredType)}|${getTypeId(initialType)}|${getSymbolId(symbol)}` : void 0;", .replacement = "if(symbol===unknownSymbol)return void 0;var __fc=flowContainer?getNodeId(flowContainer)+1:0,__dt=getTypeId(declaredType),__it=getTypeId(initialType),__si=getSymbolId(symbol);return(__dt<32768&&__si<32768)?__dt*32768+__si+1:`${__fc}|${__dt}|${__it}|${__si}`;" },
    // T7: accessibleChainCache key → packed integer
    .{ .needle = "const key = `${useOnlyExternalAliasing ? 0 : 1}|${firstRelevantLocation ? getNodeId(firstRelevantLocation) : 0}|${meaning}`;", .replacement = "const key = (useOnlyExternalAliasing?0:4194304)+(firstRelevantLocation?getNodeId(firstRelevantLocation):0)*8+meaning+1;" },
    // T8: decoratorContextOverrideType key → packed integer
    .{ .needle = "const key = `${isPrivate ? \"p\" : \"P\"}${isStatic2 ? \"s\" : \"S\"}${nameType.id}`;", .replacement = "const key = (isPrivate?2:0)+(isStatic2?1:0)+nameType.id*4+1;" },
    // T9: createSourceFile memoization
    .{ .needle = "function createSourceFile(fileName, sourceText, languageVersionOrOptions, setParentNodes = false, scriptKind) {", .replacement = "function createSourceFile(fileName, sourceText, languageVersionOrOptions, setParentNodes = false, scriptKind) {var __ck=fileName+':'+sourceText.length;if(typeof __sfCache!=='undefined'&&__sfCache[__ck])return __sfCache[__ck];" },
    // T10: createSourceFile return cache
    .{ .needle = "(_b = tracing) == null ? void 0 : _b.pop();\n  return result;\n}\nfunction parseIsolatedEntityName", .replacement = "(_b = tracing) == null ? void 0 : _b.pop();\n  if(typeof __sfCache!=='undefined')__sfCache[__ck]=result;\n  return result;\n}\nfunction parseIsolatedEntityName" },
    // T11: fileSystemEntryExists → fast callbacks
    .{ .needle = "function fileSystemEntryExists(path, entryKind) {\n      const stat = statSync(path);\n      if (!stat) {\n        return false;\n      }\n      switch (entryKind) {\n        case 0 /* File */:\n          return stat.isFile();\n        case 1 /* Directory */:\n          return stat.isDirectory();", .replacement = "function fileSystemEntryExists(path, entryKind) {\n      if(typeof __edgebox_file_exists==='function'){if(entryKind===0)return !!__edgebox_file_exists(path);if(entryKind===1)return !!__edgebox_dir_exists(path);}\n      const stat = statSync(path);\n      if (!stat) {\n        return false;\n      }\n      switch (entryKind) {\n        case 0 /* File */:\n          return stat.isFile();\n        case 1 /* Directory */:\n          return stat.isDirectory();" },
    // T12: getFlowCacheKey ThisKeyword → packed integer
    .{ .needle = "return `0|${flowContainer ? getNodeId(flowContainer) : \"-1\"}|${getTypeId(declaredType)}|${getTypeId(initialType)}`;", .replacement = "var __dt2=getTypeId(declaredType),__it2=getTypeId(initialType);return(__dt2<32768&&__it2<32768)?__dt2*32768+__it2+1:`0|${flowContainer?getNodeId(flowContainer):-1}|${__dt2}|${__it2}`;" },
    // T13: invokeOnce key → Smi
    .{ .needle = "function invokeOnce(source, target, action) {\n      const key = source.id + \",\" + target.id;", .replacement = "function invokeOnce(source, target, action) {\n      const key = (source.id<32768&&target.id<32768)?source.id*32768+target.id+1:source.id+\",\"+target.id;" },
    // T14: enumRelation key → Smi
    .{ .needle = "const id = getSymbolId(sourceSymbol) + \",\" + getSymbolId(targetSymbol);", .replacement = "const __ss=getSymbolId(sourceSymbol),__ts=getSymbolId(targetSymbol);const id=(__ss<32768&&__ts<32768)?__ss*32768+__ts+1:__ss+\",\"+__ts;" },
    // T15: homomorphicMapped key → Smi
    .{ .needle = "const cacheKey = source.id + \",\" + target.id + \",\" + constraint.id;", .replacement = "const cacheKey = (source.id<1024&&target.id<1024&&constraint.id<1024)?(source.id<<20|target.id<<10|constraint.id)+1:source.id+\",\"+target.id+\",\"+constraint.id;" },
    // T16-T20: Relation cache in Int32Array — DISABLED for A/B testing
    // The 2-way associative cache adds branch overhead on every relation check.
    // Testing whether V8's inline caching for Map.get/set is faster than our manual cache.
    // T19: relation tagging — still needed for getRelationKey discrimination
    .{ .needle = "var subtypeRelation = /* @__PURE__ */ new Map();\n  var strictSubtypeRelation = /* @__PURE__ */ new Map();\n  var assignableRelation = /* @__PURE__ */ new Map();\n  var comparableRelation = /* @__PURE__ */ new Map();\n  var identityRelation = /* @__PURE__ */ new Map();", .replacement = "var subtypeRelation = /* @__PURE__ */ new Map();subtypeRelation.__rid=1;\n  var strictSubtypeRelation = /* @__PURE__ */ new Map();strictSubtypeRelation.__rid=2;\n  var assignableRelation = /* @__PURE__ */ new Map();assignableRelation.__rid=3;\n  var comparableRelation = /* @__PURE__ */ new Map();comparableRelation.__rid=4;\n  var identityRelation = /* @__PURE__ */ new Map();identityRelation.__rid=5;" },
    // T21: getObjectFlags SOA — DISABLED: objectFlags is mutated 39 times after createType,
    // SOA column has stale data. Reading stale objectFlags causes wrong type resolution.
    // .{ .needle = "function getObjectFlags(type) {", .replacement = "..." },
    // checkSourceFile: force TurboFan optimization of hot checker functions before Check.
    // %PrepareFunctionForOptimization + %OptimizeFunctionOnNextCall = immediate TurboFan.
    // checkSourceFile: pump TurboFan before + during early Check
    .{ .needle = "forEach(host.getSourceFiles(), (file) => checkSourceFileWithEagerDiagnostics(file));", .replacement = "{if(typeof __edgebox_precompute_relations==='function')__edgebox_precompute_relations(typeCount);const __files=host.getSourceFiles();for(let __i=0;__i<__files.length;__i++){checkSourceFileWithEagerDiagnostics(__files[__i]);if(__i<30&&__i%5===4&&typeof __edgebox_precompute_relations==='function')__edgebox_precompute_relations(typeCount);}}" },
};

/// Apply all TSC source transforms in a single pass.
pub fn apply(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    var max_growth: usize = 0;
    for (&transforms) |t| {
        if (t.replacement.len > t.needle.len) {
            max_growth += (t.replacement.len - t.needle.len) * 10;
        }
    }

    const buf = try allocator.alloc(u8, source.len + max_growth);
    var pw: usize = 0;
    var pr: usize = 0;

    while (pr < source.len) {
        var matched = false;
        for (&transforms) |t| {
            if (pr + t.needle.len <= source.len and
                std.mem.startsWith(u8, source[pr..], t.needle))
            {
                @memcpy(buf[pw..][0..t.replacement.len], t.replacement);
                pw += t.replacement.len;
                pr += t.needle.len;
                matched = true;
                break;
            }
        }
        if (!matched) {
            buf[pw] = source[pr];
            pw += 1;
            pr += 1;
        }
    }

    if (pw < buf.len) {
        return try allocator.realloc(buf, pw);
    }
    return buf[0..pw];
}
