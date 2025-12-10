// Test worker_threads module

const wt = require('node:worker_threads');

console.log('isMainThread:', wt.isMainThread);
console.log('Worker class:', typeof wt.Worker);
console.log('MessageChannel class:', typeof wt.MessageChannel);
console.log('Test passed!');
