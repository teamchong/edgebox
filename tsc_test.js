console.log('[DEBUG] Starting tsc...');
try {
    require('typescript/lib/tsc.js');
    console.log('[DEBUG] tsc completed');
} catch (e) {
    console.log('[ERROR]', e.message);
    console.log(e.stack);
}
