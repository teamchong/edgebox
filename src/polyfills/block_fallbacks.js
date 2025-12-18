// Block fallback functions for partial freeze
// These are generic fallbacks that work for any function

// Generic block fallback: just re-execute the entire function
// This preserves correctness while the infrastructure is in place
globalThis.__createBlockFallback = function(funcName) {
    return function(...args) {
        // Extract: (arg0, arg1, ..., locals, block_id, stack)
        const locals = args[args.length - 3];
        const block_id = args[args.length - 2];
        const stack = args[args.length - 1];
        const originalArgs = args.slice(0, args.length - 3);

        // Get the original function
        const original = globalThis['__original_' + funcName] || globalThis[funcName];
        if (!original || typeof original !== 'function') {
            throw new Error(`Block fallback: original function '${funcName}' not found`);
        }

        // Re-execute entire function (function-level fallback for now)
        try {
            const result = original(...originalArgs);
            return { return_value: result };
        } catch (e) {
            throw e;
        }
    };
};

// Auto-generate fallbacks for known partially frozen functions
// These will be created dynamically when frozen_init() runs
