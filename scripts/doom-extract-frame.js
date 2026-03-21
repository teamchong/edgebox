// Extract Doom framebuffer from a WASM machine state result file
// and render via Kitty graphics protocol (Ghostty)
//
// Usage: edgebox doom-extract-frame.js <result-NNNN.ts> [scale]
const fs = require('fs');

const resultFile = process.argv[2];
if (!resultFile) {
  console.error('Usage: edgebox doom-extract-frame.js <result-NNNN.ts> [scale]');
  process.exit(1);
}

const scale = parseInt(process.argv[3] || '3');
const content = fs.readFileSync(resultFile, 'utf8');

// Extract memory: "BINARY_ADDR": "BYTE_VALUE"
const mem = {};
const re = /"(0[01]{31})"\s*:\s*"([01]{8})"/g;
let m;
while ((m = re.exec(content)) !== null) {
  mem[parseInt(m[1], 2)] = parseInt(m[2], 2);
}
console.error(`Memory entries: ${Object.keys(mem).length}`);

// Find framebuffer — largest dense memory region
const addrs = Object.keys(mem).map(Number).sort((a, b) => a - b);
let best = { start: 0, len: 0 }, cur = { start: addrs[0], len: 1 };
for (let i = 1; i < addrs.length; i++) {
  if (addrs[i] - addrs[i - 1] <= 2) cur.len++;
  else {
    if (cur.len > best.len) best = { ...cur };
    cur = { start: addrs[i], len: 1 };
  }
}
if (cur.len > best.len) best = { ...cur };
console.error(`Framebuffer region: 0x${best.start.toString(16)}, ${best.len} bytes`);

const W = 320, H = 200;
const fb = new Uint8Array(W * H);
for (let i = 0; i < W * H; i++) fb[i] = mem[best.start + i] || 0;

// Doom PLAYPAL palette
const P = [[0,0,0],[31,23,11],[23,15,7],[75,75,75],[255,255,255],
  [27,27,27],[19,19,19],[11,11,11],[7,7,7],[47,55,31],
  [35,43,15],[23,31,7],[15,23,0],[79,59,43],[71,51,35],
  [63,43,27],[255,183,115],[239,167,99],[223,151,83],[207,135,67],
  [191,119,55],[175,103,43],[159,91,35],[143,79,27],[127,67,19],
  [115,55,15],[99,47,11],[83,35,7],[71,27,0],[59,19,0],
  [47,15,0],[35,7,0],[27,0,0],[19,0,0],[11,0,0],
  [0,0,0],[0,0,0],[55,55,55],[63,63,63],[71,71,71],
  [79,79,79],[91,91,91],[99,99,99],[107,107,107],[119,119,119],
  [127,127,127],[135,135,135],[143,143,143],[155,155,155],[163,163,163],
  [171,171,171],[183,183,183],[191,191,191],[199,199,199],[211,211,211],
  [219,219,219],[227,227,227],[239,239,239],[247,247,247],[255,255,255]];
while (P.length < 256) { const v = P.length; P.push([v, v, v]); }

// Scale + render
const dW = W * scale, dH = H * scale;
const rgba = Buffer.alloc(dW * dH * 4);
for (let y = 0; y < dH; y++) {
  for (let x = 0; x < dW; x++) {
    const si = Math.floor(y / scale) * W + Math.floor(x / scale);
    const [r, g, b] = P[fb[si] % 256];
    const d = (y * dW + x) * 4;
    rgba[d] = r; rgba[d + 1] = g; rgba[d + 2] = b; rgba[d + 3] = 255;
  }
}

// Kitty graphics protocol
const b64 = rgba.toString('base64');
for (let i = 0; i < b64.length; i += 4096) {
  const chunk = b64.slice(i, i + 4096);
  const more = (i + 4096 < b64.length) ? 1 : 0;
  if (i === 0) process.stdout.write(`\x1b_Gf=32,s=${dW},v=${dH},a=T,m=${more};${chunk}\x1b\\`);
  else process.stdout.write(`\x1b_Gm=${more};${chunk}\x1b\\`);
}
process.stdout.write('\n');
console.error('Frame rendered');
