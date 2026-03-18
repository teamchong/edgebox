// Schema Validation Benchmark
// Validates e-commerce orders using recursive descent on a flat integer tape.
// Compares tape validation (EdgeBox WASM target) vs ajv on equivalent JS objects.
//
// Tape format (vector JSON):
//   INT=1: [1, value]
//   OBJ=2: [2, nprops, key_id, ...val, key_id, ...val, ...]
//   ARR=3: [3, nelems, ...elem, ...elem, ...]
//
// Schema: e-commerce order
//   orderId: int > 0
//   customerId: int > 0
//   status: int 0-3
//   items: array of 1-50 items
//     productId: int > 0
//     quantity: int 1-99
//     price: int 1-999999 (cents)
//     category: int 0-20
//     variants (optional): array of 1-5 variants
//       size: int 0-10
//       color: int 0-50
//       stock: int 0-99999
//   shipping: object
//     method: int 0-5
//     address: object
//       zip: int 10000-99999
//       country: int 1-250
//       region: int 0-50
//   totals: object
//     subtotal: int >= 0
//     tax: int >= 0
//     shipping: int >= 0
//     total: int >= 0
//
// Usage:
//   node bench/schema_validate.js
//   node zig-out/bin/bench/schema_validate.js/schema_validate-worker.mjs

// Key IDs (pre-mapped, like a schema registry would provide)
// Order: 1=orderId, 2=customerId, 3=status, 4=items, 5=shipping, 6=totals
// Item: 10=productId, 11=quantity, 12=price, 13=category, 14=variants
// Variant: 20=size, 21=color, 22=stock
// Shipping: 30=method, 31=address
// Address: 40=zip, 41=country, 42=region
// Totals: 50=subtotal, 51=tax, 52=shippingCost, 53=total

// === Tape validators (recursive descent — EdgeBox compiles to WASM) ===
// Each returns new position after valid data, or -1 if invalid.
// Functions defined in bottom-up order (leaves first) for cross-call detection.

function validateVariant(tape, pos) {
  if (tape[pos] !== 2) return -1;
  var nprops = tape[pos + 1 | 0] | 0;
  if (nprops !== 3) return -1;
  pos = pos + 2 | 0;
  var has = 0;
  for (var i = 0; i < nprops; i = i + 1 | 0) {
    var key = tape[pos] | 0;
    pos = pos + 1 | 0;
    if (tape[pos] !== 1) return -1;
    var val = tape[pos + 1 | 0] | 0;
    pos = pos + 2 | 0;
    if (key === 20) { if (val < 0 || val > 10) return -1; has = has | 1; }
    else if (key === 21) { if (val < 0 || val > 50) return -1; has = has | 2; }
    else if (key === 22) { if (val < 0 || val > 99999) return -1; has = has | 4; }
    else return -1;
  }
  if (has !== 7) return -1;
  return pos;
}

function validateVariants(tape, pos) {
  if (tape[pos] !== 3) return -1;
  var nelems = tape[pos + 1 | 0] | 0;
  if (nelems < 1 || nelems > 5) return -1;
  pos = pos + 2 | 0;
  for (var i = 0; i < nelems; i = i + 1 | 0) {
    pos = validateVariant(tape, pos);
    if (pos < 0) return -1;
  }
  return pos;
}

function validateAddress(tape, pos) {
  if (tape[pos] !== 2) return -1;
  var nprops = tape[pos + 1 | 0] | 0;
  if (nprops !== 3) return -1;
  pos = pos + 2 | 0;
  var has = 0;
  for (var i = 0; i < nprops; i = i + 1 | 0) {
    var key = tape[pos] | 0;
    pos = pos + 1 | 0;
    if (tape[pos] !== 1) return -1;
    var val = tape[pos + 1 | 0] | 0;
    pos = pos + 2 | 0;
    if (key === 40) { if (val < 10000 || val > 99999) return -1; has = has | 1; }
    else if (key === 41) { if (val < 1 || val > 250) return -1; has = has | 2; }
    else if (key === 42) { if (val < 0 || val > 50) return -1; has = has | 4; }
    else return -1;
  }
  if (has !== 7) return -1;
  return pos;
}

function validateTotals(tape, pos) {
  if (tape[pos] !== 2) return -1;
  var nprops = tape[pos + 1 | 0] | 0;
  if (nprops !== 4) return -1;
  pos = pos + 2 | 0;
  var has = 0;
  for (var i = 0; i < nprops; i = i + 1 | 0) {
    var key = tape[pos] | 0;
    pos = pos + 1 | 0;
    if (tape[pos] !== 1) return -1;
    var val = tape[pos + 1 | 0] | 0;
    pos = pos + 2 | 0;
    if (key === 50) { if (val < 0) return -1; has = has | 1; }
    else if (key === 51) { if (val < 0) return -1; has = has | 2; }
    else if (key === 52) { if (val < 0) return -1; has = has | 4; }
    else if (key === 53) { if (val < 0) return -1; has = has | 8; }
    else return -1;
  }
  if (has !== 15) return -1;
  return pos;
}

function validateShipping(tape, pos) {
  if (tape[pos] !== 2) return -1;
  var nprops = tape[pos + 1 | 0] | 0;
  if (nprops !== 2) return -1;
  pos = pos + 2 | 0;
  var has = 0;
  for (var i = 0; i < nprops; i = i + 1 | 0) {
    var key = tape[pos] | 0;
    pos = pos + 1 | 0;
    if (key === 30) {
      if (tape[pos] !== 1) return -1;
      var val = tape[pos + 1 | 0] | 0;
      if (val < 0 || val > 5) return -1;
      pos = pos + 2 | 0;
      has = has | 1;
    } else if (key === 31) {
      pos = validateAddress(tape, pos);
      if (pos < 0) return -1;
      has = has | 2;
    } else return -1;
  }
  if (has !== 3) return -1;
  return pos;
}

function validateItem(tape, pos) {
  if (tape[pos] !== 2) return -1;
  var nprops = tape[pos + 1 | 0] | 0;
  if (nprops < 4 || nprops > 5) return -1;
  pos = pos + 2 | 0;
  var has = 0;
  for (var i = 0; i < nprops; i = i + 1 | 0) {
    var key = tape[pos] | 0;
    pos = pos + 1 | 0;
    if (key === 14) {
      pos = validateVariants(tape, pos);
      if (pos < 0) return -1;
      has = has | 16;
    } else {
      if (tape[pos] !== 1) return -1;
      var val = tape[pos + 1 | 0] | 0;
      pos = pos + 2 | 0;
      if (key === 10) { if (val <= 0) return -1; has = has | 1; }
      else if (key === 11) { if (val < 1 || val > 99) return -1; has = has | 2; }
      else if (key === 12) { if (val < 1 || val > 999999) return -1; has = has | 4; }
      else if (key === 13) { if (val < 0 || val > 20) return -1; has = has | 8; }
      else return -1;
    }
  }
  if ((has & 15) !== 15) return -1;
  return pos;
}

function validateItems(tape, pos) {
  if (tape[pos] !== 3) return -1;
  var nelems = tape[pos + 1 | 0] | 0;
  if (nelems < 1 || nelems > 50) return -1;
  pos = pos + 2 | 0;
  for (var i = 0; i < nelems; i = i + 1 | 0) {
    pos = validateItem(tape, pos);
    if (pos < 0) return -1;
  }
  return pos;
}

function validateOrder(tape, pos) {
  if (tape[pos] !== 2) return -1;
  var nprops = tape[pos + 1 | 0] | 0;
  if (nprops !== 6) return -1;
  pos = pos + 2 | 0;
  var has = 0;
  for (var i = 0; i < nprops; i = i + 1 | 0) {
    var key = tape[pos] | 0;
    pos = pos + 1 | 0;
    if (key === 4) {
      pos = validateItems(tape, pos);
      if (pos < 0) return -1;
      has = has | 8;
    } else if (key === 5) {
      pos = validateShipping(tape, pos);
      if (pos < 0) return -1;
      has = has | 16;
    } else if (key === 6) {
      pos = validateTotals(tape, pos);
      if (pos < 0) return -1;
      has = has | 32;
    } else {
      if (tape[pos] !== 1) return -1;
      var val = tape[pos + 1 | 0] | 0;
      pos = pos + 2 | 0;
      if (key === 1) { if (val <= 0) return -1; has = has | 1; }
      else if (key === 2) { if (val <= 0) return -1; has = has | 2; }
      else if (key === 3) { if (val < 0 || val > 3) return -1; has = has | 4; }
      else return -1;
    }
  }
  if ((has & 63) !== 63) return -1;
  return pos;
}

// === Data Generation ===

function generateOrderTape(seed) {
  var tape = [];
  function pushInt(v) { tape.push(1); tape.push(v); }

  var nItems = 3 + (seed & 7); // 3-10 items
  seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;

  // Order object
  tape.push(2); tape.push(6); // OBJ, 6 props
  tape.push(1); pushInt((seed >>> 4) + 1); // orderId
  seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
  tape.push(2); pushInt((seed >>> 4) + 1); // customerId
  seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
  tape.push(3); pushInt(seed & 3); // status
  seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;

  // Items array
  tape.push(4); // key=items
  tape.push(3); tape.push(nItems); // ARR, nItems
  for (var it = 0; it < nItems; it++) {
    var hasVariants = (seed & 3) === 0; // ~25% have variants
    seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
    tape.push(2); tape.push(hasVariants ? 5 : 4); // OBJ
    tape.push(10); pushInt((seed >>> 8) + 1); // productId
    seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
    tape.push(11); pushInt(1 + (seed & 15)); // quantity
    seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
    tape.push(12); pushInt(100 + ((seed >>> 4) & 0xFFFF)); // price
    seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
    tape.push(13); pushInt(seed % 21); // category
    seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
    if (hasVariants) {
      var nVar = 1 + (seed & 1); // 1-2 variants
      seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
      tape.push(14); // key=variants
      tape.push(3); tape.push(nVar); // ARR
      for (var v = 0; v < nVar; v++) {
        tape.push(2); tape.push(3); // OBJ, 3 props
        tape.push(20); pushInt(seed & 7); // size
        seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
        tape.push(21); pushInt(seed % 40); // color
        seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
        tape.push(22); pushInt((seed >>> 4) & 0xFFFF); // stock
        seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
      }
    }
  }

  // Shipping
  tape.push(5); // key=shipping
  tape.push(2); tape.push(2); // OBJ, 2 props
  tape.push(30); pushInt(seed % 6); // method
  seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
  tape.push(31); // key=address
  tape.push(2); tape.push(3); // OBJ, 3 props
  tape.push(40); pushInt(10000 + (seed % 89999)); // zip
  seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
  tape.push(41); pushInt(1 + (seed % 200)); // country
  seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
  tape.push(42); pushInt(seed % 50); // region
  seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;

  // Totals
  tape.push(6); // key=totals
  tape.push(2); tape.push(4); // OBJ, 4 props
  var subtotal = (seed >>> 4) & 0xFFFF;
  seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF;
  var tax = subtotal / 10 | 0;
  var ship = 500 + (seed & 0xFF);
  tape.push(50); pushInt(subtotal);
  tape.push(51); pushInt(tax);
  tape.push(52); pushInt(ship);
  tape.push(53); pushInt(subtotal + tax + ship);

  return new Int32Array(tape);
}

function tapeToObject(tape) {
  var keyNames = {
    1:'orderId',2:'customerId',3:'status',4:'items',5:'shipping',6:'totals',
    10:'productId',11:'quantity',12:'price',13:'category',14:'variants',
    20:'size',21:'color',22:'stock',
    30:'method',31:'address',
    40:'zip',41:'country',42:'region',
    50:'subtotal',51:'tax',52:'shipping',53:'total'
  };
  function read(pos) {
    var type = tape[pos];
    if (type === 1) return { val: tape[pos + 1], end: pos + 2 };
    if (type === 2) {
      var obj = {}, np = tape[pos + 1]; pos += 2;
      for (var i = 0; i < np; i++) {
        var kn = keyNames[tape[pos]] || ('k' + tape[pos]); pos++;
        var r = read(pos); obj[kn] = r.val; pos = r.end;
      }
      return { val: obj, end: pos };
    }
    if (type === 3) {
      var arr = [], ne = tape[pos + 1]; pos += 2;
      for (var i = 0; i < ne; i++) {
        var r = read(pos); arr.push(r.val); pos = r.end;
      }
      return { val: arr, end: pos };
    }
    return { val: null, end: pos + 1 };
  }
  return read(0).val;
}

// === Generate test data ===
var N_ORDERS = 1000;
var tapes = [];
var objects = [];

for (var i = 0; i < N_ORDERS; i++) {
  var t = generateOrderTape(42 + i * 997);
  tapes.push(t);
  objects.push(tapeToObject(t));
}

var totalInts = 0;
for (var i = 0; i < tapes.length; i++) totalInts += tapes[i].length;
console.log('Orders: ' + N_ORDERS + ', avg tape: ' + (totalInts / N_ORDERS | 0) + ' ints, total: ' + (totalInts * 4 / 1024 | 0) + 'KB');

// Verify correctness: tape validator should accept all orders
var failures = 0;
for (var i = 0; i < N_ORDERS; i++) {
  var result = validateOrder(tapes[i], 0);
  if (result < 0) failures++;
}
if (failures > 0) console.log('ERROR: ' + failures + ' orders failed validation');

// Warmup
for (var w = 0; w < 200; w++) {
  validateOrder(tapes[w % N_ORDERS], 0);
}

// === Benchmark: Tape validation ===
var RUNS = 50;
var t0 = Date.now();
var valid = 0;
for (var r = 0; r < RUNS; r++) {
  for (var i = 0; i < N_ORDERS; i++) {
    if (validateOrder(tapes[i], 0) >= 0) valid = valid + 1 | 0;
  }
}
var tapeMs = Date.now() - t0;
console.log('tape_validate 50Kx: ' + tapeMs + 'ms (valid=' + (valid / RUNS | 0) + '/' + N_ORDERS + ')');

// === Benchmark: ajv validation (async to avoid top-level await) ===
function runAjvBenchmark(Ajv) {
  var ajv = new Ajv();

  var variantSchema = {
    type: 'object', required: ['size', 'color', 'stock'],
    properties: {
      size: { type: 'integer', minimum: 0, maximum: 10 },
      color: { type: 'integer', minimum: 0, maximum: 50 },
      stock: { type: 'integer', minimum: 0, maximum: 99999 }
    }, additionalProperties: false
  };
  var itemSchema = {
    type: 'object', required: ['productId', 'quantity', 'price', 'category'],
    properties: {
      productId: { type: 'integer', minimum: 1 },
      quantity: { type: 'integer', minimum: 1, maximum: 99 },
      price: { type: 'integer', minimum: 1, maximum: 999999 },
      category: { type: 'integer', minimum: 0, maximum: 20 },
      variants: { type: 'array', minItems: 1, maxItems: 5, items: variantSchema }
    }, additionalProperties: false
  };
  var orderSchema = {
    type: 'object', required: ['orderId', 'customerId', 'status', 'items', 'shipping', 'totals'],
    properties: {
      orderId: { type: 'integer', minimum: 1 },
      customerId: { type: 'integer', minimum: 1 },
      status: { type: 'integer', minimum: 0, maximum: 3 },
      items: { type: 'array', minItems: 1, maxItems: 50, items: itemSchema },
      shipping: {
        type: 'object', required: ['method', 'address'],
        properties: {
          method: { type: 'integer', minimum: 0, maximum: 5 },
          address: {
            type: 'object', required: ['zip', 'country', 'region'],
            properties: {
              zip: { type: 'integer', minimum: 10000, maximum: 99999 },
              country: { type: 'integer', minimum: 1, maximum: 250 },
              region: { type: 'integer', minimum: 0, maximum: 50 }
            }, additionalProperties: false
          }
        }, additionalProperties: false
      },
      totals: {
        type: 'object', required: ['subtotal', 'tax', 'shipping', 'total'],
        properties: {
          subtotal: { type: 'integer', minimum: 0 },
          tax: { type: 'integer', minimum: 0 },
          shipping: { type: 'integer', minimum: 0 },
          total: { type: 'integer', minimum: 0 }
        }, additionalProperties: false
      }
    }, additionalProperties: false
  };

  var validate = ajv.compile(orderSchema);

  // Warmup ajv
  for (var w = 0; w < 200; w++) {
    validate(objects[w % N_ORDERS]);
  }

  var at0 = Date.now();
  var avalid = 0;
  for (var r = 0; r < RUNS; r++) {
    for (var i = 0; i < N_ORDERS; i++) {
      if (validate(objects[i])) avalid = avalid + 1 | 0;
    }
  }
  var ajvMs = Date.now() - at0;
  console.log('ajv_validate 50Kx: ' + ajvMs + 'ms (valid=' + (avalid / RUNS | 0) + '/' + N_ORDERS + ')');
  console.log('tape/ajv ratio: ' + (ajvMs / tapeMs).toFixed(2) + 'x');
}

// Load ajv: try require (CJS/bundled), fallback to dynamic import (ESM)
try {
  if (typeof require === 'function') {
    var _Ajv = require('ajv').default || require('ajv');
    runAjvBenchmark(_Ajv);
  } else {
    // ESM mode: dynamic import avoids top-level await (Bun bundler compat)
    import('ajv').then(function(m) {
      runAjvBenchmark(m.default || m);
    }).catch(function(e) {
      console.log('ajv not available: ' + e.message);
    });
  }
} catch(e) {
  console.log('ajv not available: ' + e.message);
}
