import './bootstrap.js';

// Main daemon service — dispatches TSC checks to parallel checker workers
// Service bindings (WORKER_0, WORKER_1, ...) are zero-copy inside one workerd process

export default {
  async fetch(request, env) {
    var body = {};
    try { body = await request.json(); } catch(e) {}
    var projectCwd = body.cwd || '';

    if (!projectCwd) {
      var configJson = __edgebox_read_file('/tmp/edgebox-project-config.json');
      if (configJson) try { projectCwd = JSON.parse(configJson).cwd; } catch(e) {}
    }
    if (!projectCwd) return new Response('No project cwd', { status: 400 });

    // Find checker service bindings
    var checkers = [];
    for (var i = 0; i < 64; i++) {
      var c = env['WORKER_' + i];
      if (c) checkers.push(c);
      else break;
    }

    if (checkers.length === 0) {
      return new Response('No workers configured', { status: 500 });
    }

    // Dispatch to all workers in parallel
    // Each checker gets the same project but checks all files
    // (workerd service bindings = zero-copy in-process dispatch)
    var t0 = Date.now();
    var promises = checkers.map(function(checker, idx) {
      return checker.fetch(new Request('http://check/', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ cwd: projectCwd, workerId: idx, workerCount: checkers.length }),
      })).then(function(r) { return r.text(); });
    });

    var results = await Promise.all(promises);
    var checkTime = Date.now() - t0;

    // Merge diagnostics from all workers (dedup by exact line)
    var seen = {};
    var merged = [];
    for (var r = 0; r < results.length; r++) {
      var lines = results[r].split('\n');
      for (var l = 0; l < lines.length; l++) {
        if (lines[l] && !seen[lines[l]]) {
          seen[lines[l]] = true;
          merged.push(lines[l]);
        }
      }
    }

    return new Response(merged.join('\n'), {
      status: merged.length > 0 ? 422 : 200,
      headers: {
        'Content-Type': 'text/plain',
        'X-Check-Time': String(checkTime),
        'X-Workers': String(checkers.length),
        'X-Diagnostics': String(merged.length),
      }
    });
  }
};

// Fallback: single-threaded check (no checker services)
