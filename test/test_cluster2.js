// Test cluster module

const cluster = require('node:cluster');

console.log('isPrimary:', cluster.isPrimary);
console.log('isWorker:', cluster.isWorker);
console.log('fork:', typeof cluster.fork);
console.log('Test passed!');
