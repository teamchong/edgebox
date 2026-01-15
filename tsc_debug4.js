console.log('[DEBUG] Starting...');

try {
    console.log('[DEBUG] Before require');
    require('typescript/lib/tsc.js');
    console.log('[DEBUG] After require');
} catch (e) {
    console.log('[ERROR] Failed to require tsc:', e.message);
    console.log('[STACK]', e.stack);
}

console.log('[DEBUG] Done');
