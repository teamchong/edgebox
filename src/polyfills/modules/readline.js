    // ===== READLINE MODULE (lazy loaded) =====
    // Interactive line-reading interface compatible with Node.js readline
    _lazyModule('readline', function() {
        const EventEmitter = _modules.events;

        // Interface class - the main readline interface
        class Interface extends EventEmitter {
            constructor(options) {
                super();
                options = options || {};
                this.input = options.input || null;
                this.output = options.output || null;
                this.terminal = options.terminal !== false;
                this.historySize = options.historySize || 30;
                this.removeHistoryDuplicates = options.removeHistoryDuplicates || false;
                this.crlfDelay = options.crlfDelay || 100;
                this.escapeCodeTimeout = options.escapeCodeTimeout || 500;
                this.tabSize = options.tabSize || 8;
                this._prompt = '> ';
                this._closed = false;
                this._paused = false;
                this._buffer = '';
                this._history = [];
                this._historyIndex = -1;
                this._line = '';
                this._cursor = 0;

                // Setup input handling
                if (this.input) {
                    this._setupInput();
                }
            }

            _setupInput() {
                const self = this;

                if (typeof this.input.on === 'function') {
                    // Stream-based input
                    this.input.on('data', function(chunk) {
                        if (self._closed || self._paused) return;
                        self._onData(chunk);
                    });

                    this.input.on('end', function() {
                        if (self._buffer.length > 0) {
                            self._emitLine(self._buffer);
                            self._buffer = '';
                        }
                        self.close();
                    });

                    this.input.on('error', function(err) {
                        self.emit('error', err);
                    });
                }
            }

            _onData(chunk) {
                const str = typeof chunk === 'string' ? chunk : chunk.toString('utf8');
                this._buffer += str;
                this._processBuffer();
            }

            _processBuffer() {
                let newlineIdx;
                while ((newlineIdx = this._buffer.indexOf('\n')) !== -1) {
                    let line = this._buffer.slice(0, newlineIdx);
                    this._buffer = this._buffer.slice(newlineIdx + 1);

                    // Handle CRLF
                    if (line.length > 0 && line.charCodeAt(line.length - 1) === 0x0d) {
                        line = line.slice(0, -1);
                    }

                    this._emitLine(line);
                }
            }

            _emitLine(line) {
                // Add to history
                if (line.length > 0 && this.historySize > 0) {
                    if (this.removeHistoryDuplicates) {
                        const idx = this._history.indexOf(line);
                        if (idx !== -1) {
                            this._history.splice(idx, 1);
                        }
                    }
                    this._history.unshift(line);
                    if (this._history.length > this.historySize) {
                        this._history.pop();
                    }
                }
                this._historyIndex = -1;
                this.emit('line', line);
            }

            question(query, options, callback) {
                if (typeof options === 'function') {
                    callback = options;
                    options = {};
                }
                options = options || {};

                if (this._closed) {
                    const err = new Error('readline was closed');
                    err.code = 'ERR_USE_AFTER_CLOSE';
                    throw err;
                }

                // Write the query prompt
                if (this.output && query) {
                    this.output.write(query);
                }

                // Wait for next line
                const self = this;
                const handler = function(answer) {
                    if (callback) callback(answer);
                };

                if (options.signal) {
                    const signal = options.signal;
                    const abortHandler = function() {
                        self.removeListener('line', handler);
                        const err = new Error('The operation was aborted');
                        err.code = 'ABORT_ERR';
                        err.name = 'AbortError';
                        if (callback) callback(err);
                    };
                    signal.addEventListener('abort', abortHandler, { once: true });
                    this.once('line', function(answer) {
                        signal.removeEventListener('abort', abortHandler);
                        handler(answer);
                    });
                } else {
                    this.once('line', handler);
                }
            }

            prompt(preserveCursor) {
                if (this._closed) return;
                if (this.output) {
                    this.output.write(this._prompt);
                }
            }

            setPrompt(prompt) {
                this._prompt = prompt;
            }

            getPrompt() {
                return this._prompt;
            }

            close() {
                if (this._closed) return;
                this._closed = true;

                // Remove listeners from input
                if (this.input && typeof this.input.removeAllListeners === 'function') {
                    // Don't remove all - just remove our handlers
                }

                this.emit('close');
            }

            pause() {
                if (this._closed) return this;
                this._paused = true;
                if (this.input && typeof this.input.pause === 'function') {
                    this.input.pause();
                }
                this.emit('pause');
                return this;
            }

            resume() {
                if (this._closed) return this;
                this._paused = false;
                if (this.input && typeof this.input.resume === 'function') {
                    this.input.resume();
                }
                this.emit('resume');
                return this;
            }

            write(data, key) {
                if (this._closed) return;

                if (key && key.ctrl) {
                    // Handle control sequences
                    if (key.name === 'c') {
                        this.emit('SIGINT');
                        return;
                    }
                    if (key.name === 'd') {
                        this.close();
                        return;
                    }
                }

                if (data !== null && data !== undefined) {
                    this._onData(String(data));
                }
            }

            line() {
                return this._line;
            }

            cursor() {
                return this._cursor;
            }

            getCursorPos() {
                return { cols: this._cursor, rows: 0 };
            }

            // History navigation
            _historyNext() {
                if (this._historyIndex > 0) {
                    this._historyIndex--;
                    this._line = this._history[this._historyIndex] || '';
                } else if (this._historyIndex === 0) {
                    this._historyIndex = -1;
                    this._line = '';
                }
                return this._line;
            }

            _historyPrev() {
                if (this._historyIndex < this._history.length - 1) {
                    this._historyIndex++;
                    this._line = this._history[this._historyIndex] || '';
                }
                return this._line;
            }

            // Symbol.asyncIterator for for-await-of
            [Symbol.asyncIterator]() {
                const self = this;
                const lines = [];
                let resolve = null;
                let done = false;

                this.on('line', function(line) {
                    lines.push(line);
                    if (resolve) {
                        resolve();
                        resolve = null;
                    }
                });

                this.on('close', function() {
                    done = true;
                    if (resolve) {
                        resolve();
                        resolve = null;
                    }
                });

                return {
                    next: function() {
                        return new Promise(function(res) {
                            if (lines.length > 0) {
                                res({ value: lines.shift(), done: false });
                            } else if (done) {
                                res({ value: undefined, done: true });
                            } else {
                                resolve = function() {
                                    if (lines.length > 0) {
                                        res({ value: lines.shift(), done: false });
                                    } else {
                                        res({ value: undefined, done: true });
                                    }
                                };
                            }
                        });
                    },
                    return: function() {
                        self.close();
                        return Promise.resolve({ value: undefined, done: true });
                    }
                };
            }
        }

        // createInterface factory
        function createInterface(inputOrOptions, output, completer, terminal) {
            let options;

            if (inputOrOptions && typeof inputOrOptions === 'object' && !inputOrOptions.readable) {
                // Options object form
                options = inputOrOptions;
            } else {
                // Legacy form: createInterface(input, output, completer, terminal)
                options = {
                    input: inputOrOptions,
                    output: output,
                    completer: completer,
                    terminal: terminal
                };
            }

            return new Interface(options);
        }

        // Promises API
        const promises = {
            createInterface: function(options) {
                const rl = new Interface(options);

                return {
                    question: function(query, opts) {
                        return new Promise(function(resolve, reject) {
                            rl.question(query, opts, function(answer) {
                                if (answer instanceof Error) {
                                    reject(answer);
                                } else {
                                    resolve(answer);
                                }
                            });
                        });
                    },
                    close: function() {
                        rl.close();
                    },
                    [Symbol.asyncIterator]: function() {
                        return rl[Symbol.asyncIterator]();
                    }
                };
            }
        };

        // clearLine - clear entire line, before cursor (-1), or after cursor (1)
        function clearLine(stream, dir, callback) {
            if (stream && stream.write) {
                const codes = dir < 0 ? '\x1b[1K' : dir > 0 ? '\x1b[0K' : '\x1b[2K';
                stream.write(codes);
            }
            if (callback) setImmediate(callback);
            return true;
        }

        // clearScreenDown - clear screen from cursor down
        function clearScreenDown(stream, callback) {
            if (stream && stream.write) {
                stream.write('\x1b[0J');
            }
            if (callback) setImmediate(callback);
            return true;
        }

        // cursorTo - move cursor to specified position
        function cursorTo(stream, x, y, callback) {
            if (typeof y === 'function') {
                callback = y;
                y = undefined;
            }
            if (stream && stream.write) {
                if (typeof y === 'number') {
                    stream.write('\x1b[' + (y + 1) + ';' + (x + 1) + 'H');
                } else {
                    stream.write('\x1b[' + (x + 1) + 'G');
                }
            }
            if (callback) setImmediate(callback);
            return true;
        }

        // moveCursor - move cursor relative to current position
        function moveCursor(stream, dx, dy, callback) {
            if (stream && stream.write) {
                let codes = '';
                if (dx < 0) codes += '\x1b[' + (-dx) + 'D';
                else if (dx > 0) codes += '\x1b[' + dx + 'C';
                if (dy < 0) codes += '\x1b[' + (-dy) + 'A';
                else if (dy > 0) codes += '\x1b[' + dy + 'B';
                if (codes) stream.write(codes);
            }
            if (callback) setImmediate(callback);
            return true;
        }

        // emitKeypressEvents - make stream emit 'keypress' events
        function emitKeypressEvents(stream, iface) {
            if (stream._keypressDecoder) return;
            stream._keypressDecoder = true;

            stream.on('data', function(chunk) {
                const str = typeof chunk === 'string' ? chunk : chunk.toString('utf8');
                for (let i = 0; i < str.length; i++) {
                    const ch = str[i];
                    const key = {
                        sequence: ch,
                        name: ch,
                        ctrl: false,
                        meta: false,
                        shift: false
                    };

                    // Detect control characters
                    const code = ch.charCodeAt(0);
                    if (code < 32) {
                        key.ctrl = true;
                        key.name = String.fromCharCode(code + 64).toLowerCase();
                    }

                    stream.emit('keypress', ch, key);
                }
            });
        }

        return {
            Interface: Interface,
            createInterface: createInterface,
            clearLine: clearLine,
            clearScreenDown: clearScreenDown,
            cursorTo: cursorTo,
            moveCursor: moveCursor,
            emitKeypressEvents: emitKeypressEvents,
            promises: promises
        };
    });

