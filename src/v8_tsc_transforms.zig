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
    // T1: createType → populate SOA columns + trigger async flag table build
    .{ .needle = "typeCount++;\n    result.id = typeCount;", .replacement = "typeCount++;\n    result.id = typeCount;\n    if(typeof __pc_typeFlags!=='undefined'&&typeCount<262144){__pc_typeFlags[typeCount]=result.flags;if(result.objectFlags)__pc_objectFlags[typeCount]=result.objectFlags;if(typeCount===5000&&typeof __edgebox_trigger_build==='function')__edgebox_trigger_build(typeCount);}" },
    // T2: isSimpleTypeRelatedTo → read flags from SOA column
    .{ .needle = "const s = source.flags;\n    const t = target.flags;", .replacement = "const s = __pc_typeFlags[source.id|0] || source.flags;\n    const t = __pc_typeFlags[target.id|0] || target.flags;" },
    // T3: getRelationKey → packed Smi (only when postFix is empty)
    .{ .needle = "isTypeReferenceWithGenericArguments(source) && isTypeReferenceWithGenericArguments(target) ? getGenericTypeReferenceRelationKey(source, target, postFix, ignoreConstraints) : `${source.id},${target.id}${postFix}`", .replacement = "isTypeReferenceWithGenericArguments(source) && isTypeReferenceWithGenericArguments(target) ? getGenericTypeReferenceRelationKey(source, target, postFix, ignoreConstraints) : (!postFix&&source.id<32768&&target.id<32768) ? source.id * 32768 + target.id + 1 : `${source.id},${target.id}${postFix}`" },
    // T4: JSDoc skip
    .{ .needle = "jsDocParsingMode = 0", .replacement = "jsDocParsingMode = 1" },
    // T5: typeof guard for packed integer key
    .{ .needle = "id.startsWith(\"*\")", .replacement = "(typeof id === \"string\" && id.startsWith(\"*\"))" },
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
    // T16: shallow relation cache in SAB
    .{ .needle = "if (source.flags & 524288 /* Object */ && target.flags & 524288 /* Object */) {\n      const related = relation.get(getRelationKey(", .replacement = "if (source.flags & 524288 && target.flags & 524288) {\n      if(typeof __pc_relKeys!=='undefined'){var __rk=source.id*32768+target.id+1,__rb=(__rk&0x7FFFF)*6;if(__pc_relKeys[__rb]===__rk){var __rr=__pc_relKeys[__rb+2];if(__rr)return !!(__rr&1);}else if(__pc_relKeys[__rb+3]===__rk){var __rr=__pc_relKeys[__rb+5];if(__rr)return !!(__rr&1);}}\n      const related = relation.get(getRelationKey(" },
    // T17-T21: deep cache + relation tagging + getObjectFlags + checkSourceFile
    .{ .needle = "relation.set(maybeKeys[i], 1 /* Succeeded */ | propagatingVarianceFlags);", .replacement = "{var __sv=1|propagatingVarianceFlags;relation.set(maybeKeys[i],__sv);if(typeof __pc_relKeys!=='undefined'&&relation.__rid){var __sh=source2.id|(relation.__rid<<20),__sl=target2.id|(intersectionState<<20),__sb=((source2.id*32768+target2.id)&0x7FFFF)*6;if((!__pc_relKeys[__sb]&&!__pc_relKeys[__sb+1])||(__pc_relKeys[__sb]===__sh&&__pc_relKeys[__sb+1]===__sl)){__pc_relKeys[__sb]=__sh;__pc_relKeys[__sb+1]=__sl;__pc_relKeys[__sb+2]=__sv;}else if((!__pc_relKeys[__sb+3]&&!__pc_relKeys[__sb+4])||(__pc_relKeys[__sb+3]===__sh&&__pc_relKeys[__sb+4]===__sl)){__pc_relKeys[__sb+3]=__sh;__pc_relKeys[__sb+4]=__sl;__pc_relKeys[__sb+5]=__sv;}}}" },
    .{ .needle = "relation.set(id, 2 /* Failed */ | propagatingVarianceFlags);", .replacement = "{var __fv=2|propagatingVarianceFlags;relation.set(id,__fv);if(typeof __pc_relKeys!=='undefined'&&relation.__rid){var __fh=source2.id|(relation.__rid<<20),__fl=target2.id|(intersectionState<<20),__fb=((source2.id*32768+target2.id)&0x7FFFF)*6;if((!__pc_relKeys[__fb]&&!__pc_relKeys[__fb+1])||(__pc_relKeys[__fb]===__fh&&__pc_relKeys[__fb+1]===__fl)){__pc_relKeys[__fb]=__fh;__pc_relKeys[__fb+1]=__fl;__pc_relKeys[__fb+2]=__fv;}else if((!__pc_relKeys[__fb+3]&&!__pc_relKeys[__fb+4])||(__pc_relKeys[__fb+3]===__fh&&__pc_relKeys[__fb+4]===__fl)){__pc_relKeys[__fb+3]=__fh;__pc_relKeys[__fb+4]=__fl;__pc_relKeys[__fb+5]=__fv;}}}" },
    .{ .needle = "var subtypeRelation = /* @__PURE__ */ new Map();\n  var strictSubtypeRelation = /* @__PURE__ */ new Map();\n  var assignableRelation = /* @__PURE__ */ new Map();\n  var comparableRelation = /* @__PURE__ */ new Map();\n  var identityRelation = /* @__PURE__ */ new Map();", .replacement = "var subtypeRelation = /* @__PURE__ */ new Map();subtypeRelation.__rid=1;\n  var strictSubtypeRelation = /* @__PURE__ */ new Map();strictSubtypeRelation.__rid=2;\n  var assignableRelation = /* @__PURE__ */ new Map();assignableRelation.__rid=3;\n  var comparableRelation = /* @__PURE__ */ new Map();comparableRelation.__rid=4;\n  var identityRelation = /* @__PURE__ */ new Map();identityRelation.__rid=5;" },
    .{ .needle = "const entry = relation.get(id);\n      if (entry !== void 0) {", .replacement = "var entry;if(typeof __pc_relKeys!=='undefined'&&relation.__rid){var __kh=source2.id|(relation.__rid<<20),__kl=target2.id|(intersectionState<<20),__bi=((source2.id*32768+target2.id)&0x7FFFF)*6;if(__pc_relKeys[__bi]===__kh&&__pc_relKeys[__bi+1]===__kl){entry=__pc_relKeys[__bi+2];}else if(__pc_relKeys[__bi+3]===__kh&&__pc_relKeys[__bi+4]===__kl){entry=__pc_relKeys[__bi+5];}}if(entry===void 0||entry===0)entry=relation.get(id);\n      if (entry !== void 0) {" },
    .{ .needle = "relation.set(id, 2 /* Failed */ | (relationCount <= 0 ? 32 /* ComplexityOverflow */ : 64 /* StackDepthOverflow */));", .replacement = "{var __ov=2|(relationCount<=0?32:64);relation.set(id,__ov);if(typeof __pc_relKeys!=='undefined'&&relation.__rid){var __oh=source2.id|(relation.__rid<<20),__ol=target2.id|(intersectionState<<20),__ob=((source2.id*32768+target2.id)&0x7FFFF)*6;if((!__pc_relKeys[__ob]&&!__pc_relKeys[__ob+1])||(__pc_relKeys[__ob]===__oh&&__pc_relKeys[__ob+1]===__ol)){__pc_relKeys[__ob]=__oh;__pc_relKeys[__ob+1]=__ol;__pc_relKeys[__ob+2]=__ov;}else if((!__pc_relKeys[__ob+3]&&!__pc_relKeys[__ob+4])||(__pc_relKeys[__ob+3]===__oh&&__pc_relKeys[__ob+4]===__ol)){__pc_relKeys[__ob+3]=__oh;__pc_relKeys[__ob+4]=__ol;__pc_relKeys[__ob+5]=__ov;}}}" },
    .{ .needle = "function getObjectFlags(type) {\n    return type.flags & 3899393 /* ObjectFlagsType */ ? type.objectFlags : 0;\n  }", .replacement = "function getObjectFlags(type) {\n    if(typeof __pc_objectFlags!=='undefined'&&type.id>0&&type.id<262144){var __of=__pc_objectFlags[type.id];if(__of)return __of;}return type.flags & 3899393 ? type.objectFlags : 0;\n  }" },
    .{ .needle = "forEach(host.getSourceFiles(), (file) => checkSourceFileWithEagerDiagnostics(file));", .replacement = "{if(typeof __edgebox_precompute_relations==='function')__edgebox_precompute_relations(typeCount);const __files=host.getSourceFiles();const __shard=parseInt(process.env.__EDGEBOX_SHARD||'0');const __total=parseInt(process.env.__EDGEBOX_TOTAL||'1');const __start=Math.floor(__files.length*__shard/__total);const __end=Math.floor(__files.length*(__shard+1)/__total);for(let __i=__start;__i<__end;__i++){checkSourceFileWithEagerDiagnostics(__files[__i]);if(__i%50===49&&typeof __edgebox_precompute_relations==='function')__edgebox_precompute_relations(typeCount);}}" },
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
