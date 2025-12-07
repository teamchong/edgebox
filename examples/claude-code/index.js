// Claude Code Example
// Run: ./run.sh examples/claude-code/index.js -- -p "Your prompt"
//
// This is a minimal Claude Code client that demonstrates:
// - Argument parsing (scriptArgs)
// - HTTP fetch (requires server or mock)
// - stdin/stdout for interactive mode

// Parse arguments
const args = scriptArgs.slice(1); // Skip script name
let prompt = null;
let interactive = false;
let showHelp = false;

for (let i = 0; i < args.length; i++) {
    if (args[i] === '-p' || args[i] === '--prompt') {
        prompt = args[i + 1];
        i++;
    } else if (args[i] === '-i' || args[i] === '--interactive') {
        interactive = true;
    } else if (args[i] === '--help' || args[i] === '-h') {
        showHelp = true;
    }
}

// Simple Claude API client
async function chat(userMessage) {
    print("Sending to Claude...");

    // Note: HTTPS not yet supported, this is a demo
    // In production, you'd need TLS support or a proxy
    try {
        const response = await fetch("http://localhost:8080/v1/messages", {
            method: "POST",
            body: JSON.stringify({
                model: "claude-sonnet-4-20250514",
                max_tokens: 1024,
                messages: [{ role: "user", content: userMessage }]
            })
        });

        if (response.ok) {
            const data = await response.json();
            return data.content[0].text;
        } else {
            return "Error: " + response.status;
        }
    } catch (e) {
        return "Connection failed: " + e.message + "\n(Note: HTTPS not yet supported, need HTTP proxy)";
    }
}

// Main logic
async function main() {
    if (showHelp) {
        print("Claude Code - AI Assistant");
        print("");
        print("Usage: ./run.sh examples/claude-code/index.js -- [options]");
        print("");
        print("Options:");
        print("  -p, --prompt TEXT    Send a prompt to Claude");
        print("  -i, --interactive    Interactive mode (read from stdin)");
        print("  -h, --help           Show this help");
        print("");
        print("Environment:");
        print("  ANTHROPIC_API_KEY    Your Anthropic API key");
        return;
    }

    // Check for API key (use process.env polyfill)
    const apiKey = process.env.ANTHROPIC_API_KEY;
    if (!apiKey) {
        print("Warning: ANTHROPIC_API_KEY environment variable not set");
        print("Get your API key from: https://console.anthropic.com/");
    }

    if (prompt) {
        print("You: " + prompt);
        const response = await chat(prompt);
        print("\nClaude: " + response);
    } else if (interactive) {
        print("Claude Code Interactive Mode");
        print("Type your message and press Enter. Ctrl+C to exit.\n");

        while (true) {
            print("You: ", "");
            const line = process.stdin.read(4096);
            if (!line) break;

            const response = await chat(line.trim());
            print("\nClaude: " + response + "\n");
        }
    } else {
        print("Claude Code - AI Assistant");
        print("");
        print("Usage: ./run.sh examples/claude-code/index.js -- -p \"Your prompt\"");
        print("");
        print("Run with --help for more options.");
    }
}

main();
