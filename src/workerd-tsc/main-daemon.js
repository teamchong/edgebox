import './bootstrap.js';

// Main daemon — dispatch/collect via Zig shared memory. ZERO HTTP between workers.

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

    var workerCount = body.workerCount || 8;

    // Signal workers via Zig condvar (zero copy)
    __edgebox_dispatch_work(projectCwd, workerCount);

    // Block until all workers submit results (zero copy)
    var result = __edgebox_collect_results(workerCount);

    return new Response(result, {
      status: result.length > 0 ? 422 : 200,
      headers: { 'Content-Type': 'text/plain' }
    });
  }
};
