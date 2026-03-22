import './bootstrap.js';

// Main daemon — dispatches work to parallel workers via Zig channels
// Workers are separate V8 isolates in same workerd process.
// Zero HTTP between them. All via Zig shared memory + condvar.

export default {
  async fetch(request) {
    var body = {};
    try { body = await request.json(); } catch(e) {}
    var projectCwd = body.cwd || '';

    if (!projectCwd) {
      var configJson = __edgebox_read_file('/tmp/edgebox-project-config.json');
      if (configJson) try { projectCwd = JSON.parse(configJson).cwd; } catch(e) {}
    }
    if (!projectCwd) return new Response('No project cwd', { status: 400 });

    // Worker count determined by formula in Zig CLI (cpu_count / 2)
    // Read from config or use default
    var workerCount = body.workerCount || 8;

    // Dispatch work to all workers via Zig channel (zero copy)
    __edgebox_dispatch_work(projectCwd, workerCount);

    // Block until all workers done, get merged results (zero copy)
    var result = __edgebox_collect_results(workerCount);

    return new Response(result, {
      status: result.length > 0 ? 422 : 200,
      headers: { 'Content-Type': 'text/plain' }
    });
  }
};
