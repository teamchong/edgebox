// Test native path module
console.log("path.join('a', 'b', 'c'):", path.join('a', 'b', 'c'));
console.log("path.join('/a', 'b', 'c'):", path.join('/a', 'b', 'c'));
console.log("path.dirname('/foo/bar/baz.txt'):", path.dirname('/foo/bar/baz.txt'));
console.log("path.basename('/foo/bar/baz.txt'):", path.basename('/foo/bar/baz.txt'));
console.log("path.basename('/foo/bar/baz.txt', '.txt'):", path.basename('/foo/bar/baz.txt', '.txt'));
console.log("path.extname('/foo/bar/baz.txt'):", path.extname('/foo/bar/baz.txt'));
console.log("path.normalize('/foo/bar//baz/../qux'):", path.normalize('/foo/bar//baz/../qux'));
console.log("✓ Native path module works!");
