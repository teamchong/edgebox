    // ===== OS MODULE =====
    // Spoof as Darwin x64 to avoid "Unsupported architecture: wasm32" errors
    _modules.os = {
        platform: () => 'darwin',
        arch: () => 'x64',
        type: () => 'Darwin',
        release: () => '1.0.0',
        hostname: () => 'edgebox',
        homedir: () => typeof __edgebox_homedir === 'function' ? __edgebox_homedir() : '/home/user',
        tmpdir: () => '/tmp',
        cpus: () => [{ model: 'WASM', speed: 0, times: { user: 0, nice: 0, sys: 0, idle: 0, irq: 0 } }],
        totalmem: () => typeof __edgebox_totalmem === 'function' ? __edgebox_totalmem() : 4294967296,
        freemem: () => typeof __edgebox_freemem === 'function' ? __edgebox_freemem() : 2147483648,
        uptime: () => 0,
        loadavg: () => [0, 0, 0],
        networkInterfaces: () => ({}),
        userInfo: () => ({ username: 'user', uid: 1000, gid: 1000, shell: '/bin/sh', homedir: '/home/user' }),
        endianness: () => 'LE',
        EOL: '\n',
        constants: { signals: {}, errno: {} }
    };

