// Frozen TSC isSimpleTypeRelatedTo — compiled via freeze pipeline.
// Uses 3 closure imports (≤4 = get_var_ref0..3, known working in pipeline):
//   enumCheck(srcId, tgtId) → checks enum + value equality
//   objCheck(srcId, flags) → isEmpty + getObjectFlags combined
//   unknownCheck(tgtId) → isUnknownLikeUnionType

function makeFrozenChecker(enumCheck, objCheck, unknownCheck) {
  function isSimpleTypeRelatedTo(s, t, rel, sn, srcId, tgtId, wcId) {
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
  return isSimpleTypeRelatedTo;
}

function d2(a, b) { return -1; }
function d1(a) { return 0; }
var fn = makeFrozenChecker(d2, d2, d1);
var r = fn(1, 2, 0, 1, 10, 20, 999);
