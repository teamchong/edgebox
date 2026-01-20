    // ===== PERF_HOOKS MODULE =====
    // Performance measurement APIs compatible with Node.js perf_hooks

    // PerformanceEntry class
    class PerformanceEntry {
        constructor(name, entryType, startTime, duration) {
            this.name = name;
            this.entryType = entryType;
            this.startTime = startTime;
            this.duration = duration;
        }
        toJSON() {
            return {
                name: this.name,
                entryType: this.entryType,
                startTime: this.startTime,
                duration: this.duration
            };
        }
    }

    // PerformanceMark class
    class PerformanceMark extends PerformanceEntry {
        constructor(name, startTime, detail) {
            super(name, 'mark', startTime, 0);
            this.detail = detail;
        }
    }

    // PerformanceMeasure class
    class PerformanceMeasure extends PerformanceEntry {
        constructor(name, startTime, duration, detail) {
            super(name, 'measure', startTime, duration);
            this.detail = detail;
        }
    }

    // Performance class
    class Performance {
        constructor() {
            this._marks = new Map();
            this._measures = [];
            this._timeOrigin = Date.now();
        }

        get timeOrigin() {
            return this._timeOrigin;
        }

        now() {
            // High-resolution timestamp in milliseconds since timeOrigin
            // Note: We use Date.now() as base; native high-resolution timer could be added later
            return Date.now() - this._timeOrigin;
        }

        mark(name, options) {
            const startTime = options?.startTime ?? this.now();
            const detail = options?.detail;
            const entry = new PerformanceMark(name, startTime, detail);
            this._marks.set(name, entry);
            return entry;
        }

        measure(name, startMarkOrOptions, endMark) {
            let startTime, endTime, detail;

            if (typeof startMarkOrOptions === 'object' && startMarkOrOptions !== null) {
                // Options object form
                const options = startMarkOrOptions;
                detail = options.detail;
                if (options.start !== undefined) {
                    if (typeof options.start === 'string') {
                        const mark = this._marks.get(options.start);
                        startTime = mark ? mark.startTime : 0;
                    } else {
                        startTime = options.start;
                    }
                } else {
                    startTime = 0;
                }
                if (options.end !== undefined) {
                    if (typeof options.end === 'string') {
                        const mark = this._marks.get(options.end);
                        endTime = mark ? mark.startTime : this.now();
                    } else {
                        endTime = options.end;
                    }
                } else if (options.duration !== undefined) {
                    endTime = startTime + options.duration;
                } else {
                    endTime = this.now();
                }
            } else {
                // Legacy form: measure(name, startMark, endMark)
                if (startMarkOrOptions) {
                    const startMark = this._marks.get(startMarkOrOptions);
                    startTime = startMark ? startMark.startTime : 0;
                } else {
                    startTime = 0;
                }
                if (endMark) {
                    const endMarkEntry = this._marks.get(endMark);
                    endTime = endMarkEntry ? endMarkEntry.startTime : this.now();
                } else {
                    endTime = this.now();
                }
            }

            const duration = endTime - startTime;
            const entry = new PerformanceMeasure(name, startTime, duration, detail);
            this._measures.push(entry);
            return entry;
        }

        clearMarks(name) {
            if (name) {
                this._marks.delete(name);
            } else {
                this._marks.clear();
            }
        }

        clearMeasures(name) {
            if (name) {
                this._measures = this._measures.filter(m => m.name !== name);
            } else {
                this._measures = [];
            }
        }

        getEntries() {
            return [...this._marks.values(), ...this._measures];
        }

        getEntriesByName(name, type) {
            const entries = this.getEntries();
            return entries.filter(e => e.name === name && (!type || e.entryType === type));
        }

        getEntriesByType(type) {
            return this.getEntries().filter(e => e.entryType === type);
        }

        toJSON() {
            return {
                timeOrigin: this.timeOrigin
            };
        }
    }

    // PerformanceObserverEntryList class
    class PerformanceObserverEntryList {
        constructor(entries) {
            this._entries = entries || [];
        }
        getEntries() {
            return this._entries;
        }
        getEntriesByName(name, type) {
            return this._entries.filter(e => e.name === name && (!type || e.entryType === type));
        }
        getEntriesByType(type) {
            return this._entries.filter(e => e.entryType === type);
        }
    }

    // PerformanceObserver class
    class PerformanceObserver {
        constructor(callback) {
            this._callback = callback;
            this._entryTypes = [];
        }
        observe(options) {
            this._entryTypes = options?.entryTypes || [];
            // Note: In a full implementation, this would hook into the Performance instance
        }
        disconnect() {
            this._entryTypes = [];
        }
        takeRecords() {
            return [];
        }
    }

    // Create singleton performance instance
    const performanceInstance = new Performance();

    // Histogram class for measuring operations
    class Histogram {
        constructor() {
            this._values = [];
            this._min = Infinity;
            this._max = -Infinity;
        }
        get min() { return this._min === Infinity ? 0 : this._min; }
        get max() { return this._max === -Infinity ? 0 : this._max; }
        get mean() {
            if (this._values.length === 0) return 0;
            return this._values.reduce((a, b) => a + b, 0) / this._values.length;
        }
        get stddev() {
            if (this._values.length < 2) return 0;
            const mean = this.mean;
            const variance = this._values.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0) / this._values.length;
            return Math.sqrt(variance);
        }
        get count() { return this._values.length; }
        record(val) {
            this._values.push(val);
            if (val < this._min) this._min = val;
            if (val > this._max) this._max = val;
        }
        reset() {
            this._values = [];
            this._min = Infinity;
            this._max = -Infinity;
        }
        percentile(p) {
            if (this._values.length === 0) return 0;
            const sorted = [...this._values].sort((a, b) => a - b);
            const idx = Math.ceil(p / 100 * sorted.length) - 1;
            return sorted[Math.max(0, idx)];
        }
    }

    // IntervalHistogram class for event loop delay monitoring
    class IntervalHistogram extends Histogram {
        constructor(resolution) {
            super();
            this._resolution = resolution || 10; // Default 10ms resolution
            this._enabled = false;
            this._interval = null;
            this._lastCheck = 0;
        }

        enable() {
            if (this._enabled) return;
            this._enabled = true;
            this._lastCheck = Date.now();

            const self = this;
            const resolution = this._resolution;

            const check = function() {
                if (!self._enabled) return;

                const now = Date.now();
                const expected = resolution;
                const actual = now - self._lastCheck;
                const delay = Math.max(0, actual - expected);

                if (delay > 0) {
                    // Record delay in nanoseconds (Node.js convention)
                    self.record(delay * 1e6);
                }

                self._lastCheck = now;
                self._interval = setTimeout(check, resolution);
            };

            self._interval = setTimeout(check, resolution);
        }

        disable() {
            this._enabled = false;
            if (this._interval) {
                clearTimeout(this._interval);
                this._interval = null;
            }
        }

        // Node.js compatibility property accessors
        get percentiles() {
            const self = this;
            return {
                get: function(p) { return self.percentile(p); }
            };
        }

        // exceeds property - returns count of values above threshold
        get exceeds() {
            return this._values.length;
        }
    }

    // monitorEventLoopDelay factory
    function monitorEventLoopDelay(options) {
        options = options || {};
        const resolution = options.resolution || 10;
        return new IntervalHistogram(resolution);
    }

    // Module exports
    _modules.perf_hooks = {
        performance: performanceInstance,
        Performance,
        PerformanceEntry,
        PerformanceMark,
        PerformanceMeasure,
        PerformanceObserver,
        PerformanceObserverEntryList,
        Histogram,
        IntervalHistogram,
        createHistogram: () => new Histogram(),
        monitorEventLoopDelay: monitorEventLoopDelay
    };
    _modules['node:perf_hooks'] = _modules.perf_hooks;
