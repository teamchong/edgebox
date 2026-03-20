// v8_parallel_init.js — Runtime initialization for edgebox parallel APIs
// Runs AFTER __edgebox_parallel C callback is registered.
(function() {
  var e = globalThis.edgebox || {};

  // edgebox.parallel — dispatch functions to separate V8 isolates
  e.parallel = function(fns) {
    if (!Array.isArray(fns) || fns.length === 0) return [];
    var codes = fns.map(function(fn) {
      if (typeof fn === 'function') return 'return (' + fn.toString() + ')()';
      if (typeof fn === 'string') return fn;
      return 'return null';
    });
    var json = __edgebox_parallel(JSON.stringify(codes));
    return json ? JSON.parse(json) : [];
  };

  // edgebox.parallelAsync — Promise wrapper
  e.parallelAsync = function(fns) {
    return Promise.resolve(e.parallel(fns));
  };

  // edgebox.map — parallel map over arrays
  e.map = function(arr, fn, opts) {
    if (!Array.isArray(arr) || arr.length === 0) return [];
    var cpus = (opts && opts.workers) || 4;
    var size = (opts && opts.chunkSize) || Math.ceil(arr.length / cpus);
    if (arr.length <= size) return arr.map(fn); // too small to parallelize
    var chunks = [];
    for (var i = 0; i < arr.length; i += size) chunks.push(arr.slice(i, i + size));
    var fnStr = fn.toString();
    var codes = chunks.map(function(chunk) {
      return 'var fn = ' + fnStr + '; return ' + JSON.stringify(chunk) + '.map(fn)';
    });
    var json = __edgebox_parallel(JSON.stringify(codes));
    if (!json) return arr.map(fn);
    var results = JSON.parse(json);
    var flat = [];
    for (var j = 0; j < results.length; j++) {
      if (Array.isArray(results[j])) {
        for (var k = 0; k < results[j].length; k++) flat.push(results[j][k]);
      }
    }
    return flat;
  };

  // edgebox.reduce — parallel reduce with auto-combine
  e.reduce = function(arr, fn, initial, combiner) {
    if (!Array.isArray(arr) || arr.length === 0) return initial;
    var cpus = 4;
    var size = Math.ceil(arr.length / cpus);
    if (arr.length <= size) return arr.reduce(fn, initial);
    var chunks = [];
    for (var i = 0; i < arr.length; i += size) chunks.push(arr.slice(i, i + size));
    var fnStr = fn.toString();
    var initStr = JSON.stringify(initial);
    var codes = chunks.map(function(chunk) {
      return 'var fn = ' + fnStr + '; return ' + JSON.stringify(chunk) + '.reduce(fn, ' + initStr + ')';
    });
    var json = __edgebox_parallel(JSON.stringify(codes));
    if (!json) return arr.reduce(fn, initial);
    var results = JSON.parse(json);
    var comb = combiner || fn;
    var result = results[0];
    for (var j = 1; j < results.length; j++) result = comb(result, results[j]);
    return result;
  };

  // edgebox.channel — Go-like channels for worker communication
  // Usage:
  //   const ch = edgebox.channel(10);  // buffered channel
  //   ch.send(value);                  // blocks if full
  //   const val = ch.recv();           // blocks if empty, null when closed
  //   ch.close();                      // signal no more values
  if (typeof __edgebox_chan_create === 'function') {
    e.channel = function(capacity) {
      var id = __edgebox_chan_create(capacity || 1);
      if (id < 0) throw new Error('Failed to create channel');
      return {
        id: id,
        send: function(value) {
          return __edgebox_chan_send(id, JSON.stringify(value));
        },
        recv: function() {
          var json = __edgebox_chan_recv(id);
          return json !== null ? JSON.parse(json) : null;
        },
        close: function() {
          __edgebox_chan_close(id);
        },
        // Iterator protocol: for (const val of ch) { ... }
        [Symbol.iterator]: function() {
          var self = this;
          return {
            next: function() {
              var val = self.recv();
              if (val === null) return { done: true };
              return { value: val, done: false };
            }
          };
        }
      };
    };

    // edgebox.spawn — launch a function on a worker isolate (non-blocking)
    // The function receives channel wrappers as arguments
    e.spawn = function(fn, channels) {
      var chanIds = [];
      if (channels) {
        if (Array.isArray(channels)) {
          chanIds = channels.map(function(ch) { return ch.id !== undefined ? ch.id : ch; });
        } else if (channels.id !== undefined) {
          chanIds = [channels.id];
        }
      }
      var code = 'var __chIds = ' + JSON.stringify(chanIds) + ';\n' +
        'var __chs = __chIds.map(function(id) { return { id: id, ' +
        'send: function(v) { return __edgebox_chan_send(String(id), JSON.stringify(v)) === "true"; }, ' +
        'recv: function() { var j = __edgebox_chan_recv(String(id)); return j !== null ? JSON.parse(j) : null; }, ' +
        'close: function() { __edgebox_chan_close(String(id)); } }; });\n' +
        '(' + fn.toString() + ').apply(null, __chs)';
      if (typeof __edgebox_spawn === 'function') {
        __edgebox_spawn(code);
      } else {
        __edgebox_parallel(JSON.stringify([code]));
      }
    };
  }

  // edgebox.mapNumbers — zero-copy parallel map for numeric arrays
  // Input and output pass through SharedArrayBuffer (Zig mmap) — no serialization.
  // Usage: edgebox.mapNumbers(float64Array, fn) → Float64Array
  if (typeof __edgebox_chan_shared === 'function') {
    e.mapNumbers = function(arr, fn) {
      var n = arr.length;
      if (n === 0) return new Float64Array(0);
      var cpus = 4;
      var chunkSize = Math.ceil(n / cpus);
      var totalBytes = n * 8 * 2 + 16; // input + output + header

      // Allocate shared buffer: [inputCount:i32][pad:i32][pad:i32][pad:i32][input:f64×n][output:f64×n]
      var sab = __edgebox_chan_shared(String(Math.ceil(totalBytes / 8)));
      if (!sab) return Array.from(arr).map(fn); // fallback

      var header = new Int32Array(sab, 0, 4);
      var inputStart = 16;
      var outputStart = 16 + n * 8;

      // Check if SAB is large enough
      if (sab.byteLength < outputStart + n * 8) {
        return Array.from(arr).map(fn); // fallback — buffer too small
      }

      var input = new Float64Array(sab, inputStart, n);
      var output = new Float64Array(sab, outputStart, n);

      // Copy input to shared buffer (one copy — from JS heap to mmap)
      if (arr instanceof Float64Array) {
        input.set(arr);
      } else {
        for (var i = 0; i < n; i++) input[i] = arr[i];
      }
      Atomics.store(header, 0, n); // store input count

      // Build worker code that reads from shared buffer, applies fn, writes output
      var fnStr = fn.toString();
      var codes = [];
      for (var c = 0; c < cpus; c++) {
        var start = c * chunkSize;
        var end = Math.min(start + chunkSize, n);
        if (start >= end) break;
        // Worker gets SAB via __edgebox_chan_get_shared, applies fn to each element
        codes.push(
          'var sab = __edgebox_chan_get_shared("' + (header[3] || 0) + '");' +
          'if(!sab)return null;' +
          'var inp = new Float64Array(sab, ' + inputStart + ', ' + n + ');' +
          'var out = new Float64Array(sab, ' + outputStart + ', ' + n + ');' +
          'var fn = ' + fnStr + ';' +
          'for(var i=' + start + ';i<' + end + ';i++)out[i]=fn(inp[i],i);' +
          'return ' + (end - start)
        );
      }

      // Store channel ID for workers to find
      // The channel ID is the index used to create this SAB
      // Workers use __edgebox_chan_get_shared to get the same SAB view

      var results = JSON.parse(__edgebox_parallel(JSON.stringify(codes)));

      // Read output directly from shared buffer — zero-copy!
      var result = new Float64Array(n);
      result.set(output);
      return result;
    };
  }

  // edgebox.sharedChannel — zero-copy channel using SharedArrayBuffer
  // Data passes through Zig-managed shared memory without serialization.
  // Layout: [head:i32][tail:i32][closed:i32][pad:i32][data:f64 × capacity]
  // Uses Atomics for lock-free synchronization.
  if (typeof __edgebox_chan_shared === 'function') {
    e.sharedChannel = function(capacity) {
      capacity = capacity || 1024;
      var sab = __edgebox_chan_shared(String(capacity));
      if (!sab) throw new Error('Failed to create shared channel');
      var header = new Int32Array(sab, 0, 4); // [head, tail, closed, channelId]
      var data = new Float64Array(sab, 16);    // f64 slots after header
      var cap = data.length;
      // Store channel ID in header[3] for workers to look up
      var chId = Atomics.load(header, 3); // set by Zig
      return {
        sab: sab,
        send: function(value) {
          while (true) {
            var tail = Atomics.load(header, 1);
            var head = Atomics.load(header, 0);
            var count = (tail - head + cap) % cap;
            if (count >= cap - 1) {
              // Buffer full — spin wait (could use Atomics.wait but needs i32 view)
              continue;
            }
            data[tail % cap] = value;
            Atomics.store(header, 1, (tail + 1) % cap);
            Atomics.notify(header, 0, 1); // wake receiver
            return true;
          }
        },
        recv: function() {
          while (true) {
            var head = Atomics.load(header, 0);
            var tail = Atomics.load(header, 1);
            if (head === tail) {
              if (Atomics.load(header, 2) === 1) return null; // closed
              Atomics.wait(header, 0, head, 1); // wait 1ms then retry
              continue;
            }
            var value = data[head % cap];
            Atomics.store(header, 0, (head + 1) % cap);
            Atomics.notify(header, 1, 1); // wake sender
            return value;
          }
        },
        close: function() {
          Atomics.store(header, 2, 1);
          Atomics.notify(header, 0, 1);
        },
        get length() {
          var t = Atomics.load(header, 1);
          var h = Atomics.load(header, 0);
          return (t - h + cap) % cap;
        }
      };
    };
  }

  globalThis.edgebox = e;
})();
