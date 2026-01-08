// Minimal WASI shim for browser
// Implements only what QuickJS needs: fd_write for console output

export class BrowserWASI {
  constructor() {
    this.args = ['edgebox'];
    this.env = [];
    this.stdout = '';
    this.stderr = '';
  }

  // WASI imports
  get wasiImport() {
    return {
      // args_get
      args_get: (argv, argvBuf) => {
        const mem = new Uint8Array(this.memory.buffer);
        const view = new DataView(this.memory.buffer);
        let offset = argvBuf;
        for (let i = 0; i < this.args.length; i++) {
          view.setUint32(argv + i * 4, offset, true);
          const arg = new TextEncoder().encode(this.args[i] + '\0');
          mem.set(arg, offset);
          offset += arg.length;
        }
        return 0;
      },

      // args_sizes_get
      args_sizes_get: (argc, argvBufSize) => {
        const view = new DataView(this.memory.buffer);
        view.setUint32(argc, this.args.length, true);
        const size = this.args.reduce((sum, arg) => sum + arg.length + 1, 0);
        view.setUint32(argvBufSize, size, true);
        return 0;
      },

      // environ_get
      environ_get: () => 0,

      // environ_sizes_get
      environ_sizes_get: (environCount, environBufSize) => {
        const view = new DataView(this.memory.buffer);
        view.setUint32(environCount, 0, true);
        view.setUint32(environBufSize, 0, true);
        return 0;
      },

      // clock_time_get - for performance.now()
      clock_time_get: (clockId, precision, time) => {
        const view = new DataView(this.memory.buffer);
        const now = BigInt(Math.floor(performance.now() * 1000000)); // nanoseconds
        view.setBigUint64(time, now, true);
        return 0;
      },

      // fd_write - for console output
      fd_write: (fd, iovs, iovsLen, nwritten) => {
        const mem = new Uint8Array(this.memory.buffer);
        const view = new DataView(this.memory.buffer);
        let written = 0;
        let output = '';

        for (let i = 0; i < iovsLen; i++) {
          const ptr = view.getUint32(iovs + i * 8, true);
          const len = view.getUint32(iovs + i * 8 + 4, true);
          const bytes = mem.slice(ptr, ptr + len);
          output += new TextDecoder().decode(bytes);
          written += len;
        }

        if (fd === 1) {
          this.stdout += output;
          console.log(output.trimEnd());
        } else if (fd === 2) {
          this.stderr += output;
          console.error(output.trimEnd());
        }

        view.setUint32(nwritten, written, true);
        return 0;
      },

      // fd_close
      fd_close: () => 0,

      // fd_seek
      fd_seek: () => 70, // ESPIPE

      // fd_read
      fd_read: () => 0,

      // fd_fdstat_get
      fd_fdstat_get: (fd, stat) => {
        const view = new DataView(this.memory.buffer);
        // filetype: 2 = character device
        view.setUint8(stat, 2);
        // flags
        view.setUint16(stat + 2, 0, true);
        // rights_base
        view.setBigUint64(stat + 8, 0n, true);
        // rights_inheriting
        view.setBigUint64(stat + 16, 0n, true);
        return 0;
      },

      // proc_exit
      proc_exit: (code) => {
        throw new Error(`WASI exit: ${code}`);
      },

      // random_get
      random_get: (buf, len) => {
        const mem = new Uint8Array(this.memory.buffer);
        const random = new Uint8Array(len);
        crypto.getRandomValues(random);
        mem.set(random, buf);
        return 0;
      },
    };
  }

  setMemory(memory) {
    this.memory = memory;
  }
}

// Load and run WASM
export async function runEdgeboxWasm(wasmUrl) {
  const wasi = new BrowserWASI();

  const response = await fetch(wasmUrl);
  const wasmBytes = await response.arrayBuffer();

  const imports = {
    wasi_snapshot_preview1: wasi.wasiImport,
  };

  const { instance } = await WebAssembly.instantiate(wasmBytes, imports);

  wasi.setMemory(instance.exports.memory);

  try {
    instance.exports._start();
  } catch (e) {
    if (!e.message.startsWith('WASI exit:')) {
      throw e;
    }
  }

  return { stdout: wasi.stdout, stderr: wasi.stderr };
}
