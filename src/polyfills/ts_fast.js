// TypeScript Fast Path Polyfill
// Wraps ts.transpileModule to use native fast path when possible

(function() {
    // Only install if fast transpile is available
    if (typeof __edgebox_fast_transpile !== 'function') {
        return;
    }

    // Store original transpileModule
    const originalTranspileModule = globalThis.ts?.transpileModule;

    // Fast path wrapper
    function fastTranspileModule(source, options) {
        // Try native fast path first
        const fastResult = __edgebox_fast_transpile(source);

        if (fastResult !== null) {
            // Fast path succeeded - return compatible result object
            return {
                outputText: fastResult,
                diagnostics: [],
                sourceMapText: undefined
            };
        }

        // Fall back to full TypeScript compiler
        if (originalTranspileModule) {
            return originalTranspileModule.call(globalThis.ts, source, options);
        }

        // No TypeScript available - throw error
        throw new Error('TypeScript compiler not available and fast path failed');
    }

    // Install fast path wrapper
    if (globalThis.ts) {
        globalThis.ts.transpileModule = fastTranspileModule;
    }

    // Also expose as global for before TS is loaded
    globalThis.__edgebox_fast_transpileModule = fastTranspileModule;
})();
