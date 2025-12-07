// Minimal EventEmitter polyfill for EdgeBox

function EventEmitter() {
  this._events = {};
  this._eventsCount = 0;
  this._maxListeners = undefined;
}

EventEmitter.prototype.on = function(event, listener) {
  if (!this._events[event]) {
    this._events[event] = [];
    this._eventsCount++;
  }
  this._events[event].push(listener);
  return this;
};

EventEmitter.prototype.addListener = EventEmitter.prototype.on;

EventEmitter.prototype.once = function(event, listener) {
  var self = this;
  function wrapper() {
    self.removeListener(event, wrapper);
    listener.apply(self, arguments);
  }
  wrapper.listener = listener;
  return this.on(event, wrapper);
};

EventEmitter.prototype.prependListener = function(event, listener) {
  if (!this._events[event]) {
    this._events[event] = [];
    this._eventsCount++;
  }
  this._events[event].unshift(listener);
  return this;
};

EventEmitter.prototype.prependOnceListener = function(event, listener) {
  var self = this;
  function wrapper() {
    self.removeListener(event, wrapper);
    listener.apply(self, arguments);
  }
  wrapper.listener = listener;
  return this.prependListener(event, wrapper);
};

EventEmitter.prototype.off = function(event, listener) {
  return this.removeListener(event, listener);
};

EventEmitter.prototype.removeListener = function(event, listener) {
  if (!this._events[event]) return this;
  var list = this._events[event];
  for (var i = list.length - 1; i >= 0; i--) {
    if (list[i] === listener || list[i].listener === listener) {
      list.splice(i, 1);
      if (list.length === 0) {
        delete this._events[event];
        this._eventsCount--;
      }
      break;
    }
  }
  return this;
};

EventEmitter.prototype.removeAllListeners = function(event) {
  if (event) {
    if (this._events[event]) {
      delete this._events[event];
      this._eventsCount--;
    }
  } else {
    this._events = {};
    this._eventsCount = 0;
  }
  return this;
};

EventEmitter.prototype.emit = function(event) {
  if (!this._events[event]) return false;
  var args = Array.prototype.slice.call(arguments, 1);
  var listeners = this._events[event].slice();
  for (var i = 0; i < listeners.length; i++) {
    try {
      listeners[i].apply(this, args);
    } catch (err) {
      console.error('EventEmitter error:', err);
    }
  }
  return true;
};

EventEmitter.prototype.listeners = function(event) {
  return this._events[event] ? this._events[event].slice() : [];
};

EventEmitter.prototype.listenerCount = function(event) {
  return this._events[event] ? this._events[event].length : 0;
};

EventEmitter.prototype.eventNames = function() {
  return Object.keys(this._events);
};

EventEmitter.prototype.setMaxListeners = function(n) {
  this._maxListeners = n;
  return this;
};

EventEmitter.prototype.getMaxListeners = function() {
  return this._maxListeners !== undefined ? this._maxListeners : EventEmitter.defaultMaxListeners;
};

// Static properties and methods
EventEmitter.defaultMaxListeners = 10;

EventEmitter.listenerCount = function(emitter, event) {
  return emitter.listenerCount(event);
};

EventEmitter.once = function(emitter, event) {
  return new Promise(function(resolve, reject) {
    function errorListener(err) {
      emitter.removeListener(event, resolver);
      reject(err);
    }
    function resolver() {
      emitter.removeListener('error', errorListener);
      resolve(Array.prototype.slice.call(arguments));
    }
    emitter.once(event, resolver);
    emitter.once('error', errorListener);
  });
};

EventEmitter.EventEmitter = EventEmitter;

module.exports = EventEmitter;
