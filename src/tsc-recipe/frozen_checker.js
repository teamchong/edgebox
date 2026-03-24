// Frozen TSC checker — isRelatedTo + isSimpleTypeRelatedTo combined.
// One big function that handles identity + normalization + flag checks.
// Falls through to JS for structural/recursive cases.

function makeFrozenChecker(enumCheck, objCheck, unknownCheck) {
  // Combined isSimpleTypeRelatedTo (inlined)
  function simpleCheck(s, t, rel, sn, srcId, tgtId, wcId) {
    if (t & 1 || s & 131072 || srcId === wcId) return 1;
    if (t & 2 && !(rel === 3 && s & 1)) return 1;
    if (t & 131072) return 0;
    if (s & 402653316 && t & 4) return 1;
    if (s & 128 && s & 1024 && t & 128 && !(t & 1024)) {
      return enumCheck(srcId, tgtId);
    }
    if (s & 296 && t & 8) return 1;
    if (s & 256 && s & 1024 && t & 256 && !(t & 1024)) {
      return enumCheck(srcId, tgtId);
    }
    if (s & 2112 && t & 64) return 1;
    if (s & 528 && t & 16) return 1;
    if (s & 12288 && t & 4096) return 1;
    if (s & 32 && t & 32) {
      return enumCheck(srcId, tgtId);
    }
    if (s & 1024 && t & 1024) {
      return enumCheck(srcId, tgtId);
    }
    if (s & 32768 && (sn === 0 && !(t & 3145728) || t & 49152)) return 1;
    if (s & 65536 && (sn === 0 && !(t & 3145728) || t & 65536)) return 1;
    if (s & 524288 && t & 67108864) {
      if (rel === 3) return objCheck(srcId, 0);
      return 1;
    }
    if (rel === 0 || rel === 1) {
      if (s & 1) return 1;
      if (s & 8 && (t & 32 || t & 256 && t & 1024)) return 1;
      if (s & 256 && !(s & 1024) && (t & 32 || t & 256 && t & 1024)) {
        return enumCheck(srcId, tgtId);
      }
      if (unknownCheck(tgtId)) return 1;
      return -1;
    }
    return -1;
  }

  // isRelatedTo fast path — inlines simpleCheck + handles normalization
  // Returns: 1=related, 0=not related, -1=unknown (fall through to JS)
  function isRelatedToFast(sf, tf, rel, sn, srcId, tgtId, wcId) {
    // Identity
    if (srcId === tgtId) return 1;
    // Object → Primitive: run simpleCheck both ways
    if (sf & 524288 && tf & 402784252) {
      if (rel === 1 && !(tf & 131072)) {
        var r1 = simpleCheck(tf, sf, rel, sn, tgtId, srcId, wcId);
        if (r1 === 1) return 1;
      }
      var r2 = simpleCheck(sf, tf, rel, sn, srcId, tgtId, wcId);
      if (r2 === 1) return 1;
      if (r2 === 0) return 0;
      return -1;
    }
    // Identity relation: flags must match exactly
    if (rel === 4) {
      if (sf !== tf) return 0;
      if (sf & 67358815) return 1;
      return -1;
    }
    // Simple flag check
    var r = simpleCheck(sf, tf, rel, sn, srcId, tgtId, wcId);
    if (r !== -1) return r;
    // Structured/recursive → fall through to JS
    return -1;
  }

  return isRelatedToFast;
}

function d2(a, b) { return -1; }
function d1(a) { return 0; }
var fn = makeFrozenChecker(d2, d2, d1);
var r = fn(1, 2, 0, 1, 10, 20, 999);
