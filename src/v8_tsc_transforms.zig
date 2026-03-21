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
    // T1: createType → assign __fid (flag ID) for bitmap lookup.
    // Maps type.flags → small integer ID. ~200 distinct flag values for 66K types.
    .{ .needle = "typeCount++;\n    result.id = typeCount;", .replacement = "typeCount++;\n    result.id = typeCount;\n    if(typeof __fidMap!=='undefined'){var __f=result.flags,__fid=__fidMap[__f];if(__fid===undefined){__fid=__fidNext++;__fidMap[__f]=__fid;if(__fid<256&&typeof __pc_typeFlags!=='undefined')__pc_typeFlags[__fid]=__f;}result.__fid=__fid;}" },
    // T2: isTypeRelatedTo → bitmap lookup (pre-computed by Zig SIMD).
    // bitmap[source.__fid * 256 + target.__fid] = 1 (related) or 0 (unknown).
    // Falls back to inline flag checks if bitmap miss or __fid not set.
    .{ .needle = "function isTypeRelatedTo(source, target, relation) {\n    if (isFreshLiteralType(source)) {\n      source = source.regularType;\n    }\n    if (isFreshLiteralType(target)) {\n      target = target.regularType;\n    }\n    if (source === target) {\n      return true;\n    }", .replacement = "function isTypeRelatedTo(source, target, relation) {\n    if (isFreshLiteralType(source)) {\n      source = source.regularType;\n    }\n    if (isFreshLiteralType(target)) {\n      target = target.regularType;\n    }\n    if (source === target) {\n      return true;\n    }\n    if(typeof __flagBitmap!=='undefined'&&source.__fid!==undefined&&target.__fid!==undefined){var __br=__flagBitmap[source.__fid*256+target.__fid];if(__br===1)return true;}\n    var __tf=target.flags;if(__tf&1)return true;var __sf=source.flags;if(__sf&131072)return true;if(__sf&402653316&&__tf&4||__sf&296&&__tf&8||__sf&2112&&__tf&64||__sf&528&&__tf&16||__sf&12288&&__tf&4096)return true;" },
    // T3: getRelationKey → packed Smi when IDs fit (< 32768) and no postFix.
    .{ .needle = "isTypeReferenceWithGenericArguments(source) && isTypeReferenceWithGenericArguments(target) ? getGenericTypeReferenceRelationKey(source, target, postFix, ignoreConstraints) : `${source.id},${target.id}${postFix}`", .replacement = "isTypeReferenceWithGenericArguments(source) && isTypeReferenceWithGenericArguments(target) ? getGenericTypeReferenceRelationKey(source, target, postFix, ignoreConstraints) : (!postFix&&source.id<32768&&target.id<32768) ? source.id * 32768 + target.id + 1 : `${source.id},${target.id}${postFix}`" },
    // T4: JSDoc skip
    .{ .needle = "jsDocParsingMode = 0", .replacement = "jsDocParsingMode = 1" },
    // T5: typeof guard for packed integer key
    .{ .needle = "id.startsWith(\"*\")", .replacement = "(typeof id === \"string\" && id.startsWith(\"*\"))" },
    // T-SOA1/T-SOA2 disabled for A/B test — checking if property count
    // fast-reject is net-positive or if the typeof/bounds overhead hurts.
    // T-NODE: SOA write for node.kind — DISABLED: populated but never read.
    // T-SYMID: SOA write for symbol.flags — DISABLED: populated but never read.
    // T6: getFlowCacheKey Identifier → packed integer
    .{ .needle = "return symbol !== unknownSymbol ? `${flowContainer ? getNodeId(flowContainer) : \"-1\"}|${getTypeId(declaredType)}|${getTypeId(initialType)}|${getSymbolId(symbol)}` : void 0;", .replacement = "if(symbol===unknownSymbol)return void 0;var __fc=flowContainer?getNodeId(flowContainer)+1:0,__dt=getTypeId(declaredType),__it=getTypeId(initialType),__si=getSymbolId(symbol);return(__dt<32768&&__si<32768)?__dt*32768+__si+1:`${__fc}|${__dt}|${__it}|${__si}`;" },
    // T7: accessibleChainCache key → packed integer
    .{ .needle = "const key = `${useOnlyExternalAliasing ? 0 : 1}|${firstRelevantLocation ? getNodeId(firstRelevantLocation) : 0}|${meaning}`;", .replacement = "const key = (useOnlyExternalAliasing?0:4194304)+(firstRelevantLocation?getNodeId(firstRelevantLocation):0)*8+meaning+1;" },
    // T8: decoratorContextOverrideType key → packed integer
    .{ .needle = "const key = `${isPrivate ? \"p\" : \"P\"}${isStatic2 ? \"s\" : \"S\"}${nameType.id}`;", .replacement = "const key = (isPrivate?2:0)+(isStatic2?1:0)+nameType.id*4+1;" },
    // T9: createSourceFile memoization + pump TurboFan during Parse.
    // Pumps every 100 files to let TurboFan compile checker functions
    // while parsing is still in progress. Reduces Check phase JIT variance.
    .{ .needle = "function createSourceFile(fileName, sourceText, languageVersionOrOptions, setParentNodes = false, scriptKind) {", .replacement = "function createSourceFile(fileName, sourceText, languageVersionOrOptions, setParentNodes = false, scriptKind) {var __ck=fileName+':'+sourceText.length;if(typeof __sfCache!=='undefined'&&__sfCache[__ck])return __sfCache[__ck];if(typeof __edgebox_precompute_relations==='function'&&typeof __sfParseCount==='undefined')globalThis.__sfParseCount=0;if(typeof __sfParseCount!=='undefined'&&++__sfParseCount%100===0&&typeof __edgebox_precompute_relations==='function')__edgebox_precompute_relations(0);" },
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
    // Before Check: warm hot checker functions by calling with real types,
    // then pump to flush TurboFan compilations. Reduces JIT variance.
    // checkSourceFile: pump TurboFan before Check + periodic pump during early files
    // T-PARALLEL: Spawn workers AFTER main's createProgram.
    // All workers have full programs — correct diagnostics.
    .{ .needle = "const program = createProgram(programOptions);\n  const exitStatus = emitFilesAndReportErrorsAndGetExitStatus(", .replacement = "const program = createProgram(programOptions);\n  if(typeof __edgebox_spawn_check_workers==='function')__edgebox_spawn_check_workers();\n  const exitStatus = emitFilesAndReportErrorsAndGetExitStatus(" },
    // T-SHARD-DIAGS: Filter diagnostics by worker shard.
    // Each worker only reports diagnostics from its assigned files.
    // Without this, every worker reports ALL diagnostics (duplicates).
    .{ .needle = "const diagnostics = sortAndDeduplicateDiagnostics(allDiagnostics);\n  diagnostics.forEach(reportDiagnostic);", .replacement = "var diagnostics = sortAndDeduplicateDiagnostics(allDiagnostics);\n  if(typeof __edgebox_worker_count!=='undefined'&&__edgebox_worker_count>1){var __sf=program.getSourceFiles();var __myFiles=new Set();for(var __di=0;__di<__sf.length;__di++){if(__di%__edgebox_worker_count===__edgebox_worker_id)__myFiles.add(__sf[__di].fileName);}diagnostics=diagnostics.filter(function(d){return !d.file||__myFiles.has(d.file.fileName);});}\n  diagnostics.forEach(reportDiagnostic);" },
    // Check loop: shard files across parallel workers.
    // Each worker checks files where fileIndex % workerCount === workerId.
    // When not set (single-threaded), defaults to id=0, count=1 → checks all files.
    .{ .needle = "forEach(host.getSourceFiles(), (file) => checkSourceFileWithEagerDiagnostics(file));", .replacement = "{if(typeof __edgebox_precompute_relations==='function')__edgebox_precompute_relations(typeCount);const __files=host.getSourceFiles();const __wid=typeof __edgebox_worker_id!=='undefined'?__edgebox_worker_id:0;const __wcnt=typeof __edgebox_worker_count!=='undefined'?__edgebox_worker_count:1;for(let __i=0;__i<__files.length;__i++){if(__i%__wcnt===__wid){checkSourceFileWithEagerDiagnostics(__files[__i]);}if(__i<30&&__i%5===4&&typeof __edgebox_precompute_relations==='function')__edgebox_precompute_relations(typeCount);}}" },
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
