const fs = require('fs');

// Standard Doom PLAYPAL (first 256 colors, RGB)
// Source: extracted from DOOM.WAD PLAYPAL lump
const DOOM_PALETTE = [
  [0,0,0],[31,23,11],[23,15,7],[75,75,75],[255,255,255],
  [27,27,27],[19,19,19],[11,11,11],[7,7,7],[47,55,31],
  [35,43,15],[23,31,7],[15,23,0],[79,59,43],[71,51,35],
  [63,43,27],[255,183,115],[239,167,99],[223,151,83],[207,135,67],
  [191,119,55],[175,103,43],[159,91,35],[143,79,27],[127,67,19],
  [115,55,15],[99,47,11],[83,35,7],[71,27,0],[59,19,0],
  [47,15,0],[35,7,0],[27,0,0],[0,0,0],[0,0,0],
  [0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0],
  // ... simplified palette — full 256 entries would be here
  // For demo, use grayscale based on index
];

// Fill remaining entries with grayscale
for (let i = DOOM_PALETTE.length; i < 256; i++) {
  const v = Math.floor(i * 255 / 256);
  DOOM_PALETTE.push([v, v, v]);
}

// Read palette values from the result file
const palettePath = process.argv[2] || 'packages/playground/final-doom-pun-intended/palette-values.ts';
const content = fs.readFileSync(palettePath, 'utf8');
const values = content.match(/\d+/g).map(Number);

const width = 320;
const height = Math.min(200, Math.floor(values.length / width));
console.error(`Frame: ${width}x${height}, ${values.length} pixels`);

// Create raw RGBA pixel data
const pixels = Buffer.alloc(width * height * 4);
for (let i = 0; i < width * height && i < values.length; i++) {
  const idx = values[i] % 256;
  const [r, g, b] = DOOM_PALETTE[idx];
  pixels[i * 4] = r;
  pixels[i * 4 + 1] = g;
  pixels[i * 4 + 2] = b;
  pixels[i * 4 + 3] = 255;
}

// Render via Kitty graphics protocol (Ghostty compatible)
// Format: \x1b_Gf=32,s=<width>,v=<height>,a=T;BASE64\x1b\\
const b64 = pixels.toString('base64');

// Send in chunks (Kitty protocol max chunk size)
const CHUNK = 4096;
for (let i = 0; i < b64.length; i += CHUNK) {
  const chunk = b64.slice(i, i + CHUNK);
  const more = (i + CHUNK < b64.length) ? 1 : 0;
  if (i === 0) {
    process.stdout.write(`\x1b_Gf=32,s=${width},v=${height},a=T,m=${more};${chunk}\x1b\\`);
  } else {
    process.stdout.write(`\x1b_Gm=${more};${chunk}\x1b\\`);
  }
}
process.stdout.write('\n');
console.error('Frame rendered via Kitty graphics protocol');
