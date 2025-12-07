// Minimal util polyfill for EdgeBox

function format(f) {
  if (typeof f !== 'string') {
    var objects = [];
    for (var i = 0; i < arguments.length; i++) {
      objects.push(inspect(arguments[i]));
    }
    return objects.join(' ');
  }
  var args = Array.prototype.slice.call(arguments, 1);
  var i = 0;
  var str = String(f).replace(/%[sdjoO%]/g, function(x) {
    if (x === '%%') return '%';
    if (i >= args.length) return x;
    var v = args[i++];
    switch (x) {
      case '%s': return String(v);
      case '%d': return Number(v);
      case '%j': case '%o': case '%O':
        try { return JSON.stringify(v); } catch(e) { return '[Circular]'; }
      default: return x;
    }
  });
  return str;
}

function inspect(obj, opts) {
  if (obj === null) return 'null';
  if (obj === undefined) return 'undefined';
  if (typeof obj === 'function') return '[Function' + (obj.name ? ': ' + obj.name : '') + ']';
  if (typeof obj === 'string') return "'" + obj + "'";
  if (typeof obj !== 'object') return String(obj);
  try {
    return JSON.stringify(obj, null, 2);
  } catch(e) {
    return '[object Object]';
  }
}

function deprecate(fn, msg) {
  var warned = false;
  return function() {
    if (!warned) {
      console.warn(msg);
      warned = true;
    }
    return fn.apply(this, arguments);
  };
}

function debuglog(section) {
  return function() {};
}

function inherits(ctor, superCtor) {
  if (superCtor) {
    ctor.super_ = superCtor;
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  }
}

function _extend(target, source) {
  if (!source || typeof source !== 'object') return target;
  var keys = Object.keys(source);
  for (var i = 0; i < keys.length; i++) {
    target[keys[i]] = source[keys[i]];
  }
  return target;
}

function promisify(fn) {
  return function() {
    var args = Array.prototype.slice.call(arguments);
    return new Promise(function(resolve, reject) {
      args.push(function(err, result) {
        if (err) reject(err);
        else resolve(result);
      });
      fn.apply(null, args);
    });
  };
}

function callbackify(fn) {
  return function() {
    var args = Array.prototype.slice.call(arguments);
    var cb = args.pop();
    fn.apply(null, args).then(
      function(r) { cb(null, r); },
      function(e) { cb(e); }
    );
  };
}

function log() {
  console.log(format.apply(null, arguments));
}

// Type checking functions
function isArray(arg) { return Array.isArray(arg); }
function isBoolean(arg) { return typeof arg === 'boolean'; }
function isNull(arg) { return arg === null; }
function isNullOrUndefined(arg) { return arg == null; }
function isNumber(arg) { return typeof arg === 'number'; }
function isString(arg) { return typeof arg === 'string'; }
function isSymbol(arg) { return typeof arg === 'symbol'; }
function isUndefined(arg) { return arg === undefined; }
function isRegExp(re) { return re instanceof RegExp; }
function isObject(arg) { return typeof arg === 'object' && arg !== null; }
function isDate(d) { return d instanceof Date; }
function isError(e) { return e instanceof Error; }
function isFunction(arg) { return typeof arg === 'function'; }
function isPrimitive(arg) {
  return arg === null || (typeof arg !== 'object' && typeof arg !== 'function');
}
function isBuffer(arg) {
  return arg && typeof arg === 'object' && typeof arg.copy === 'function' &&
         typeof arg.fill === 'function' && typeof arg.readUInt8 === 'function';
}

var types = {
  isRegExp: isRegExp,
  isDate: isDate,
  isNativeError: isError,
  isArray: isArray,
  isBoolean: isBoolean,
  isNull: isNull,
  isNullOrUndefined: isNullOrUndefined,
  isNumber: isNumber,
  isString: isString,
  isSymbol: isSymbol,
  isUndefined: isUndefined,
  isObject: isObject,
  isFunction: isFunction,
  isBuffer: isBuffer,
  isPrimitive: isPrimitive
};

module.exports = {
  format: format,
  inspect: inspect,
  deprecate: deprecate,
  debuglog: debuglog,
  inherits: inherits,
  _extend: _extend,
  promisify: promisify,
  callbackify: callbackify,
  log: log,
  types: types,
  isArray: isArray,
  isBoolean: isBoolean,
  isNull: isNull,
  isNullOrUndefined: isNullOrUndefined,
  isNumber: isNumber,
  isString: isString,
  isSymbol: isSymbol,
  isUndefined: isUndefined,
  isRegExp: isRegExp,
  isObject: isObject,
  isDate: isDate,
  isError: isError,
  isFunction: isFunction,
  isPrimitive: isPrimitive,
  isBuffer: isBuffer,
  TextEncoder: globalThis.TextEncoder,
  TextDecoder: globalThis.TextDecoder
};
