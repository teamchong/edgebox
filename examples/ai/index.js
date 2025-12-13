// AI Chat Example - Uses WASI-NN for local LLM inference
//
// Prerequisites:
//   1. Build EdgeBox with WASI-NN support:
//      zig build wasm -Doptimize=ReleaseFast -Denable-wasi-nn=true
//
//   2. Download a GGUF model (e.g., Llama 3.1):
//      curl -LO https://huggingface.co/second-state/Meta-Llama-3.1-8B-Instruct-GGUF/resolve/main/Meta-Llama-3.1-8B-Instruct-Q5_K_M.gguf
//
// Run:
//   zig-out/bin/edgebox examples/ai/index.js

// Check if AI is available
if (typeof ai !== 'undefined' && ai.isAvailable()) {
    console.log('WASI-NN AI is available!');

    // Get prompt from command line or use default
    const prompt = process.argv[2] || 'What is WebAssembly?';
    console.log('\nPrompt:', prompt);
    console.log('\nGenerating response...\n');

    try {
        const response = ai.chat(prompt);
        console.log('Response:', response);
    } catch (err) {
        console.error('Error:', err.message);
    }
} else {
    console.log('WASI-NN AI is not available.');
    console.log('\nTo enable AI inference:');
    console.log('  1. Build with WASI-NN: zig build wasm -Denable-wasi-nn=true');
    console.log('  2. Download a model:');
    console.log('     curl -LO https://huggingface.co/second-state/Meta-Llama-3.1-8B-Instruct-GGUF/resolve/main/Meta-Llama-3.1-8B-Instruct-Q5_K_M.gguf');
}
