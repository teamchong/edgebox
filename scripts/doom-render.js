const fs = require('fs');

// Doom PLAYPAL approximation (256 RGB colors)
const P = [];
const raw = [0,0,0, 31,23,11, 23,15,7, 75,75,75, 255,255,255,
  27,27,27, 19,19,19, 11,11,11, 7,7,7, 47,55,31,
  35,43,15, 23,31,7, 15,23,0, 79,59,43, 71,51,35,
  63,43,27, 255,183,115, 239,167,99, 223,151,83, 207,135,67,
  191,119,55, 175,103,43, 159,91,35, 143,79,27, 127,67,19,
  115,55,15, 99,47,11, 83,35,7, 71,27,0, 59,19,0,
  47,15,0, 35,7,0, 27,0,0, 19,0,0, 11,0,0,
  0,0,0, 0,0,0, 55,55,55, 63,63,63, 71,71,71,
  79,79,79, 91,91,91, 99,99,99, 107,107,107, 119,119,119,
  127,127,127, 135,135,135, 143,143,143, 155,155,155, 163,163,163,
  171,171,171, 183,183,183, 191,191,191, 199,199,199, 211,211,211,
  219,219,219, 227,227,227, 239,239,239, 247,247,247, 255,255,255,
  119,95,59, 111,87,55, 103,79,47, 95,75,43, 87,67,39,
  79,63,35, 71,55,31, 63,51,27, 55,43,23, 47,39,19,
  39,31,15, 31,27,11, 23,19,7, 15,15,7, 7,7,0,
  183,107,43, 175,99,35, 167,91,27, 159,83,23, 151,79,19,
  143,71,15, 135,63,11, 127,55,7, 119,47,0, 111,43,0,
  103,35,0, 95,31,0, 87,23,0, 79,19,0, 71,15,0, 63,11,0];
for (let i = 0; i < raw.length; i += 3) P.push([raw[i], raw[i+1], raw[i+2]]);
while (P.length < 256) { const v = Math.floor(P.length * 255 / 256); P.push([v, Math.floor(v*0.8), Math.floor(v*0.6)]); }

const palettePath = process.argv[2];
if (!palettePath) { console.error('Usage: edgebox doom-render.js <palette-values.ts> [scale]'); process.exit(1); }

const scale = parseInt(process.argv[3] || '3'); // 3x = 960×600
const content = fs.readFileSync(palettePath, 'utf8');
const values = content.match(/\d+/g).map(Number);

const srcW = 320, srcH = Math.min(200, Math.floor(values.length / 320));
const dstW = srcW * scale, dstH = srcH * scale;

// Scale up with nearest-neighbor
const pixels = Buffer.alloc(dstW * dstH * 4);
for (let y = 0; y < dstH; y++) {
  const srcY = Math.floor(y / scale);
  for (let x = 0; x < dstW; x++) {
    const srcX = Math.floor(x / scale);
    const srcIdx = srcY * srcW + srcX;
    const idx = values[srcIdx] % 256;
    const [r, g, b] = P[idx];
    const dst = (y * dstW + x) * 4;
    pixels[dst] = r; pixels[dst+1] = g; pixels[dst+2] = b; pixels[dst+3] = 255;
  }
}

// Kitty graphics protocol — use image ID 1 so frames replace in-place
const b64 = pixels.toString('base64');
const CHUNK = 4096;
// Delete previous image (suppress Ghostty response by not using quiet=0)
for (let i = 0; i < b64.length; i += CHUNK) {
  const chunk = b64.slice(i, i + CHUNK);
  const more = (i + CHUNK < b64.length) ? 1 : 0;
  if (i === 0) {
    process.stdout.write(`\x1b_Gi=1,f=32,s=${dstW},v=${dstH},a=T,m=${more};${chunk}\x1b\\`);
  } else {
    process.stdout.write(`\x1b_Gm=${more};${chunk}\x1b\\`);
  }
}
process.stdout.write('\n');
