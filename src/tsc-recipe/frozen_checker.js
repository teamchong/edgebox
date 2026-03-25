function checkFlags(sf, tf, rel, sn) {
  if (tf & 1 || sf & 131072) return 1;
  if (tf & 2 && !(rel === 3 && sf & 1)) return 1;
  if (tf & 131072) return 0;
  if (sf & 402653316 && tf & 4) return 1;
  if (sf & 128 && sf & 1024 && tf & 128 && !(tf & 1024)) return -1;
  if (sf & 296 && tf & 8) return 1;
  if (sf & 256 && sf & 1024 && tf & 256 && !(tf & 1024)) return -1;
  if (sf & 2112 && tf & 64) return 1;
  if (sf & 528 && tf & 16) return 1;
  if (sf & 12288 && tf & 4096) return 1;
  if (sf & 32 && tf & 32) return -1;
  if (sf & 1024 && tf & 1024) return -1;
  if (sf & 32768 && (sn === 0 || tf & 49152)) return 1;
  if (sf & 65536 && (sn === 0 || tf & 65536)) return 1;
  if (sf & 524288 && tf & 67108864) return -1;
  if (rel === 0 || rel === 1) {
    if (sf & 1) return 1;
    if (sf & 8 && (tf & 32 || tf & 256 && tf & 1024)) return 1;
    if (sf & 256 && !(sf & 1024) && (tf & 32 || tf & 256 && tf & 1024)) return -1;
  }
  return -1;
}
function isRelatedToFast(srcId, tgtId, sf, tf, rel, sn) {
  if (srcId === tgtId) return 1;
  if (rel === 4) {
    if (sf !== tf) return 0;
    if (sf & 67358815) return 1;
    return -1;
  }
  var chk = checkFlags(sf, tf, rel, sn);
  if (chk !== -1) return chk;
  if ((sf & 249860) && (tf & 249860) && !(sf & tf & 249860)) return 0;
  return -1;
}
globalThis._r = isRelatedToFast(1, 2, 4, 8, 0, 1);

function getObjectFlagsF(typeFlags, objectFlags) {
  return typeFlags & 3899393 ? objectFlags : 0;
}

function bloomReject(srcBloom, tgtBloom) {
  if (srcBloom === 0 || tgtBloom === 0) return 0;
  return (tgtBloom & (~srcBloom)) !== 0 ? 1 : 0;
}

var _t2 = getObjectFlagsF(524288, 16);
var _t3 = bloomReject(7, 15);
