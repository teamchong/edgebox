// Test cluster module

const cluster = require('cluster');

console.log('isPrimary:', cluster.isPrimary);
console.log('isWorker:', cluster.isWorker);
console.log('fork:', typeof cluster.fork);

if (cluster.isPrimary) {
    console.log('This is the primary process');
    console.log('Test passed - cluster module loads correctly');
} else {
    console.log('This is a worker process, id:', cluster.worker.id);
}
