// Test worker_threads module

const { Worker, isMainThread, parentPort, workerData } = require('worker_threads');

console.log('isMainThread:', isMainThread);

if (isMainThread) {
    console.log('Main thread - Worker class:', typeof Worker);
    console.log('Test passed - worker_threads module loads correctly');
} else {
    console.log('Worker thread - parentPort:', parentPort);
    console.log('Worker thread - workerData:', workerData);
}
