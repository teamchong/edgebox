// Comprehensive Path polyfill tests
// Tests path.join, resolve, dirname, basename, extname, normalize, parse, format

const {
    assertEqual, assertTrue, assertFalse, assertDeepEqual, summary
} = require('./helpers/assert.js');

const path = require('path');

console.log('=== Path Polyfill Tests ===\n');

// ============================================
// path.sep and path.delimiter
// ============================================
console.log('--- path.sep and path.delimiter ---');
assertEqual(path.sep, '/', 'path.sep is /');
assertEqual(path.delimiter, ':', 'path.delimiter is :');

// ============================================
// path.join
// ============================================
console.log('\n--- path.join ---');

assertEqual(path.join('foo', 'bar', 'baz'), 'foo/bar/baz', 'join basic');
assertEqual(path.join('/foo', 'bar', 'baz'), '/foo/bar/baz', 'join absolute');
assertEqual(path.join('foo', '', 'bar'), 'foo/bar', 'join with empty');
assertEqual(path.join('foo', 'bar', ''), 'foo/bar', 'join trailing empty');
assertEqual(path.join(''), '.', 'join empty only');
assertEqual(path.join('.', 'foo'), 'foo', 'join with dot');
assertEqual(path.join('foo', '.', 'bar'), 'foo/bar', 'join with middle dot');

// With .. navigation
assertEqual(path.join('foo', 'bar', '..', 'baz'), 'foo/baz', 'join with ..');
assertEqual(path.join('foo', 'bar', '..', '..', 'baz'), 'baz', 'join with multiple ..');
assertEqual(path.join('/foo', 'bar', '..'), '/foo', 'join absolute with ..');
assertEqual(path.join('..', 'foo'), '../foo', 'join starting with ..');

// Multiple slashes
assertEqual(path.join('foo/', '/bar'), 'foo/bar', 'join removes double slashes');
assertEqual(path.join('foo//', 'bar'), 'foo/bar', 'join normalizes multiple slashes');

// ============================================
// path.resolve
// ============================================
console.log('\n--- path.resolve ---');

// Resolve with absolute path
assertEqual(path.resolve('/foo', 'bar'), '/foo/bar', 'resolve basic');
assertEqual(path.resolve('/foo', '/bar'), '/bar', 'resolve with absolute overrides');
assertEqual(path.resolve('foo', '/bar', 'baz'), '/bar/baz', 'resolve middle absolute');

// Resolve with dots
assertEqual(path.resolve('/foo', '.', 'bar'), '/foo/bar', 'resolve with dot');
assertEqual(path.resolve('/foo', 'bar', '..', 'baz'), '/foo/baz', 'resolve with ..');
assertEqual(path.resolve('/foo', 'bar', '..', '..'), '/', 'resolve to root');

// Empty parts
assertEqual(path.resolve('/foo', '', 'bar'), '/foo/bar', 'resolve with empty');

// ============================================
// path.normalize
// ============================================
console.log('\n--- path.normalize ---');

assertEqual(path.normalize('/foo/bar//baz'), '/foo/bar/baz', 'normalize double slash');
assertEqual(path.normalize('/foo/bar/./baz'), '/foo/bar/baz', 'normalize with dot');
assertEqual(path.normalize('/foo/bar/../baz'), '/foo/baz', 'normalize with ..');
assertEqual(path.normalize('/foo/bar/baz/..'), '/foo/bar', 'normalize trailing ..');
assertEqual(path.normalize('/foo/../foo/bar'), '/foo/bar', 'normalize back and forth');
assertEqual(path.normalize('foo//bar'), 'foo/bar', 'normalize relative');
assertEqual(path.normalize('./foo'), 'foo', 'normalize leading dot');
assertEqual(path.normalize('../foo'), '../foo', 'normalize leading ..');
assertEqual(path.normalize('foo/..'), '.', 'normalize to current');
assertEqual(path.normalize('/'), '/', 'normalize root');
assertEqual(path.normalize('.'), '.', 'normalize dot');

// ============================================
// path.dirname
// ============================================
console.log('\n--- path.dirname ---');

assertEqual(path.dirname('/foo/bar/baz'), '/foo/bar', 'dirname basic');
assertEqual(path.dirname('/foo/bar'), '/foo', 'dirname level 2');
assertEqual(path.dirname('/foo'), '/', 'dirname to root');
assertEqual(path.dirname('/'), '/', 'dirname of root');
assertEqual(path.dirname('foo/bar'), 'foo', 'dirname relative');
assertEqual(path.dirname('foo'), '.', 'dirname single');
assertEqual(path.dirname(''), '.', 'dirname empty');

// With extension
assertEqual(path.dirname('/foo/bar.js'), '/foo', 'dirname with extension');

// ============================================
// path.basename
// ============================================
console.log('\n--- path.basename ---');

assertEqual(path.basename('/foo/bar/baz'), 'baz', 'basename basic');
assertEqual(path.basename('/foo/bar.js'), 'bar.js', 'basename with extension');
assertEqual(path.basename('/foo/bar.js', '.js'), 'bar', 'basename remove extension');
assertEqual(path.basename('/foo/bar.js', '.txt'), 'bar.js', 'basename wrong extension');
assertEqual(path.basename('/foo/bar/'), 'bar', 'basename trailing slash');
assertEqual(path.basename('foo'), 'foo', 'basename relative');
assertEqual(path.basename('/'), '', 'basename root');
assertEqual(path.basename(''), '', 'basename empty');

// ============================================
// path.extname
// ============================================
console.log('\n--- path.extname ---');

assertEqual(path.extname('foo.js'), '.js', 'extname basic');
assertEqual(path.extname('/foo/bar.js'), '.js', 'extname with path');
assertEqual(path.extname('foo.bar.js'), '.js', 'extname multiple dots');
assertEqual(path.extname('foo'), '', 'extname no extension');
assertEqual(path.extname('.hidden'), '', 'extname hidden file');
assertEqual(path.extname('foo.'), '.', 'extname trailing dot');
assertEqual(path.extname('/foo/bar'), '', 'extname directory');
assertEqual(path.extname(''), '', 'extname empty');
assertEqual(path.extname('.gitignore'), '', 'extname dotfile');
assertEqual(path.extname('file.tar.gz'), '.gz', 'extname double extension');

// ============================================
// path.isAbsolute
// ============================================
console.log('\n--- path.isAbsolute ---');

assertTrue(path.isAbsolute('/foo/bar'), 'isAbsolute /foo/bar');
assertTrue(path.isAbsolute('/'), 'isAbsolute /');
assertFalse(path.isAbsolute('foo/bar'), 'isAbsolute foo/bar');
assertFalse(path.isAbsolute('./foo'), 'isAbsolute ./foo');
assertFalse(path.isAbsolute('../foo'), 'isAbsolute ../foo');
assertFalse(path.isAbsolute(''), 'isAbsolute empty');

// ============================================
// path.relative
// ============================================
console.log('\n--- path.relative ---');

assertEqual(path.relative('/foo/bar', '/foo/bar/baz'), 'baz', 'relative child');
assertEqual(path.relative('/foo/bar/baz', '/foo/bar'), '..', 'relative parent');
assertEqual(path.relative('/foo/bar', '/foo/qux'), '../qux', 'relative sibling');
assertEqual(path.relative('/foo/bar/baz', '/foo/qux/quux'), '../../qux/quux', 'relative cousin');
// relative same returns '.' or '' depending on implementation
const relSame = path.relative('/foo/bar', '/foo/bar');
assertTrue(relSame === '.' || relSame === '', 'relative same returns . or empty');
assertEqual(path.relative('/foo', '/bar'), '../bar', 'relative different roots');

// ============================================
// path.parse
// ============================================
console.log('\n--- path.parse ---');

let parsed = path.parse('/home/user/file.js');
assertEqual(parsed.root, '/', 'parse root');
assertEqual(parsed.dir, '/home/user', 'parse dir');
assertEqual(parsed.base, 'file.js', 'parse base');
assertEqual(parsed.ext, '.js', 'parse ext');
assertEqual(parsed.name, 'file', 'parse name');

parsed = path.parse('/home/user/');
assertEqual(parsed.root, '/', 'parse dir root');
assertEqual(parsed.base, 'user', 'parse dir base');

parsed = path.parse('file.js');
assertEqual(parsed.root, '', 'parse relative root');
// dir for relative file is '.' or '' depending on implementation
assertTrue(parsed.dir === '.' || parsed.dir === '', 'parse relative dir is . or empty');
assertEqual(parsed.base, 'file.js', 'parse relative base');

parsed = path.parse('/');
assertEqual(parsed.root, '/', 'parse root only root');
assertEqual(parsed.base, '', 'parse root only base');

// ============================================
// path.format
// ============================================
console.log('\n--- path.format ---');

assertEqual(path.format({ dir: '/home/user', base: 'file.js' }), '/home/user/file.js', 'format dir+base');
assertEqual(path.format({ root: '/', base: 'file.js' }), '/file.js', 'format root+base');
assertEqual(path.format({ dir: '/home/user', name: 'file', ext: '.js' }), '/home/user/file.js', 'format dir+name+ext');
assertEqual(path.format({ name: 'file', ext: '.js' }), 'file.js', 'format name+ext only');
assertEqual(path.format({ root: '/', dir: '/home/user', base: 'file.js' }), '/home/user/file.js', 'format all parts');

// ============================================
// path.parse/format round-trip
// ============================================
console.log('\n--- parse/format round-trip ---');

function roundTrip(p) {
    return path.format(path.parse(p));
}

assertEqual(roundTrip('/home/user/file.js'), '/home/user/file.js', 'round-trip absolute');
assertEqual(roundTrip('file.js'), 'file.js', 'round-trip relative');
assertEqual(roundTrip('/home/user/'), '/home/user', 'round-trip directory');

// ============================================
// path.posix
// ============================================
console.log('\n--- path.posix ---');

if (path.posix) {
    assertEqual(path.posix.sep, '/', 'posix.sep');
    assertEqual(path.posix.delimiter, ':', 'posix.delimiter');
    assertEqual(path.posix.join('foo', 'bar'), 'foo/bar', 'posix.join');
    assertEqual(path.posix.basename('/foo/bar.js'), 'bar.js', 'posix.basename');
}

// ============================================
// Edge Cases
// ============================================
console.log('\n--- Edge Cases ---');

// Many slashes (trailing slash handling varies by implementation)
const normMany = path.normalize('///foo///bar///');
assertTrue(normMany.startsWith('/foo/bar'), 'normalize many slashes starts correctly');
const joinMany = path.join('///', 'foo', '///bar///');
assertTrue(joinMany.startsWith('/foo/bar'), 'join many slashes starts correctly');

// Deep nesting
const deep = '/a/b/c/d/e/f/g/h/i/j';
assertEqual(path.dirname(path.dirname(path.dirname(deep))), '/a/b/c/d/e/f/g', 'deep dirname');

// Long path
const longPath = '/' + Array(100).fill('segment').join('/');
assertTrue(path.normalize(longPath).startsWith('/'), 'long path normalize');
assertEqual(path.basename(longPath), 'segment', 'long path basename');

// Special characters in path
assertEqual(path.join('foo', 'bar-baz'), 'foo/bar-baz', 'join with dash');
assertEqual(path.join('foo', 'bar_baz'), 'foo/bar_baz', 'join with underscore');
assertEqual(path.basename('foo.bar.baz.js'), 'foo.bar.baz.js', 'multiple dots basename');
assertEqual(path.extname('foo.bar.baz.js'), '.js', 'multiple dots extname');

// Spaces in paths
assertEqual(path.join('foo bar', 'baz'), 'foo bar/baz', 'join with space');
assertEqual(path.basename('/foo/bar baz.js'), 'bar baz.js', 'basename with space');

summary();
