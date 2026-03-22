import './bootstrap.js';

// Main daemon — dispatches parallel check via Zig green threads
// __edgebox_parallel_run spawns N Zig threads, each runs workerd child
// File cache shared via Zig mmap. Zero HTTP between workers.

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

    // Dispatch to Zig parallel checker (N threads, each runs workerd child)
    var cpuCount = 4; // formula applied in Zig side
    var result = __edgebox_parallel_run(projectCwd, cpuCount);

    return new Response(result, {
      status: result.length > 0 ? 422 : 200,
      headers: { 'Content-Type': 'text/plain' }
    });
  }
};
