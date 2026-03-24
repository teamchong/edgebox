// V8 Bridge: Create TSC SourceFile from Zig flat binary AST
//
// Reads flat AST buffer (24 bytes/node) and creates TSC Node tree.
// Uses a simple approach: create nodes as plain objects with the
// properties that TSC's binder/checker access via forEachChild.
//
// Called from recipe as: __zigCreateSourceFile(sourceText, fileName)

(function() {
  if (!globalThis.ts) return;
  var ts = globalThis.ts;

  // Read flat node from ArrayBuffer at index
  function readNode(view, idx) {
    var off = idx * 24;
    return {
      kind: view.getUint16(off, true),
      flags: view.getUint16(off + 2, true),
      start: view.getUint32(off + 4, true),
      end: view.getUint32(off + 8, true),
      parent: view.getUint32(off + 12, true),
      firstChild: view.getUint32(off + 16, true),
      nextSibling: view.getUint32(off + 20, true),
    };
  }

  // Get all children of a flat node
  function getChildren(view, nodeCount, idx) {
    var children = [];
    var flat = readNode(view, idx);
    var child = flat.firstChild;
    while (child !== 0xFFFFFFFF && child < nodeCount) {
      children.push(child);
      var childFlat = readNode(view, child);
      child = childFlat.nextSibling;
    }
    return children;
  }

  // Cache the Node constructor for fast object creation
  var NodeCtor = ts.objectAllocator.getNodeConstructor();

  // Create a TSC-compatible node from flat AST
  // Uses the MonoNode constructor (pre-initializes 19 properties in one call)
  function createTSCNode(view, nodeCount, sourceText, idx, parentNode) {
    var flat = readNode(view, idx);
    // MonoNode(kind, pos, end) — fastest V8 object creation path
    var node = new NodeCtor(flat.kind, flat.start, flat.end);
    node.flags = flat.flags;
    node.parent = parentNode || undefined;

    var children = getChildren(view, nodeCount, idx);
    var childNodes = [];
    for (var i = 0; i < children.length; i++) {
      childNodes.push(createTSCNode(view, nodeCount, sourceText, children[i], node));
    }

    // Map children to named properties based on SyntaxKind
    mapChildrenToProperties(node, childNodes, flat.kind);

    return node;
  }

  // Map flat children to named properties expected by TSC's forEachChild
  function mapChildrenToProperties(node, children, kind) {
    switch (kind) {
      case 316: // SourceFile — should NOT be created via this path
        node.statements = createNodeArray(children);
        break;
      case 243: // VariableStatement
        if (children.length > 0) node.declarationList = children[0];
        break;
      case 261: // VariableDeclarationList
        node.declarations = createNodeArray(children);
        break;
      case 260: // VariableDeclaration
        if (children.length > 0) node.name = children[0];
        if (children.length > 1) node.initializer = children[1];
        break;
      case 244: // ExpressionStatement
        if (children.length > 0) node.expression = children[0];
        break;
      case 245: // IfStatement
        if (children.length > 0) node.expression = children[0];
        if (children.length > 1) node.thenStatement = children[1];
        if (children.length > 2) node.elseStatement = children[2];
        break;
      case 253: // ReturnStatement
        if (children.length > 0) node.expression = children[0];
        break;
      case 241: // Block
        node.statements = createNodeArray(children);
        break;
      case 262: // FunctionDeclaration
        if (children.length > 0) node.name = children[0];
        node.parameters = createNodeArray([]);
        if (children.length > 1) node.body = children[children.length - 1];
        break;
      case 263: // ClassDeclaration
        if (children.length > 0) node.name = children[0];
        node.members = createNodeArray(children.slice(1));
        break;
      case 264: // InterfaceDeclaration
        if (children.length > 0) node.name = children[0];
        node.members = createNodeArray(children.slice(1));
        break;
      case 265: // TypeAliasDeclaration
        if (children.length > 0) node.name = children[0];
        break;
      case 272: // ImportDeclaration
        break; // Import details parsed by Zig are minimal
      case 278: // ExportDeclaration
        break;
      case 226: // BinaryExpression
        if (children.length > 0) node.left = children[0];
        if (children.length > 1) node.right = children[1];
        break;
      case 213: // CallExpression
        if (children.length > 0) node.expression = children[0];
        node.arguments = createNodeArray(children.slice(1));
        break;
      case 211: // PropertyAccessExpression
        if (children.length > 0) node.expression = children[0];
        if (children.length > 1) node.name = children[1];
        break;
      case 219: // ArrowFunction
        node.parameters = createNodeArray([]);
        if (children.length > 0) node.body = children[children.length - 1];
        break;
      default:
        // Generic: store all children
        if (children.length > 0) {
          node.expression = children[0];
        }
        break;
    }
  }

  function createNodeArray(nodes) {
    var arr = nodes.slice();
    arr.pos = nodes.length > 0 ? nodes[0].pos : -1;
    arr.end = nodes.length > 0 ? nodes[nodes.length - 1].end : -1;
    arr.hasTrailingComma = false;
    arr.transformFlags = 0;
    return arr;
  }

  // Single-pass node creation: create ALL nodes first, then link children.
  // Avoids recursive calls — flat loop is faster.
  globalThis.__zigCreateSourceFile = function(sourceText, fileName, flatASTBuffer) {
    if (!flatASTBuffer || flatASTBuffer.byteLength < 24) {
      return ts.createSourceFile(fileName, sourceText, 99, true);
    }

    var view = new DataView(flatASTBuffer);
    var nodeCount = flatASTBuffer.byteLength / 24;

    // Phase 1: Create ALL nodes in a flat array (single pass, no recursion)
    var tscNodes = new Array(nodeCount);
    for (var i = 0; i < nodeCount; i++) {
      var off = i * 24;
      var kind = view.getUint16(off, true);
      var start = view.getUint32(off + 4, true);
      var end = view.getUint32(off + 8, true);
      tscNodes[i] = new NodeCtor(kind, start, end);
      tscNodes[i].flags = view.getUint16(off + 2, true);
    }

    // Phase 2: Link parent-child relationships
    for (var j = 0; j < nodeCount; j++) {
      var off2 = j * 24;
      var parentIdx = view.getUint32(off2 + 12, true);
      if (parentIdx !== 0xFFFFFFFF && parentIdx < nodeCount) {
        tscNodes[j].parent = tscNodes[parentIdx];
      }
    }

    // Phase 3: Collect children for each node and assign named properties
    for (var k = 0; k < nodeCount; k++) {
      var off3 = k * 24;
      var firstChild = view.getUint32(off3 + 16, true);
      if (firstChild === 0xFFFFFFFF) continue;

      var children = [];
      var ch = firstChild;
      while (ch !== 0xFFFFFFFF && ch < nodeCount) {
        children.push(tscNodes[ch]);
        var chOff = ch * 24;
        ch = view.getUint32(chOff + 20, true);
      }
      mapChildrenToProperties(tscNodes[k], children, tscNodes[k].kind);
    }

    // Phase 4: Build SourceFile wrapper
    var SFCtor = ts.objectAllocator.getSourceFileConstructor();
    var sf = new SFCtor(316, 0, sourceText.length);
    sf.text = sourceText;
    sf.fileName = fileName;
    sf.path = fileName.toLowerCase();
    sf.resolvedPath = fileName.toLowerCase();
    sf.originalFileName = fileName;
    sf.languageVersion = 99;
    sf.languageVariant = 0;
    sf.scriptKind = fileName.endsWith('.tsx') ? 4 : fileName.endsWith('.jsx') ? 2 : 3;
    sf.isDeclarationFile = fileName.endsWith('.d.ts');
    sf.hasNoDefaultLib = false;
    sf.parseDiagnostics = [];
    sf.bindDiagnostics = [];
    sf.pragmas = new Map();
    sf.referencedFiles = [];
    sf.typeReferenceDirectives = [];
    sf.libReferenceDirectives = [];
    sf.amdDependencies = [];
    sf.identifiers = new Map();
    sf.nodeCount = nodeCount;
    sf.identifierCount = 0;
    sf.symbolCount = 0;

    // DEBUG: verify flat AST structure
    if (typeof __edgebox_write_stderr === 'function') {
      __edgebox_write_stderr('[bridge] node0: kind=' + view.getUint16(0, true) +
        ' firstChild=' + view.getUint32(16, true) +
        ' nodeCount=' + nodeCount + '\n');
      if (nodeCount > 1) {
        __edgebox_write_stderr('[bridge] node1: kind=' + view.getUint16(24, true) +
          ' nextSibling=' + view.getUint32(24 + 20, true) + '\n');
      }
    }

    // Get root node's children as statements
    var rootChildren = [];
    var rootFirstOff = 0 * 24 + 16;
    var rootFirst = view.getUint32(rootFirstOff, true);
    var rc = rootFirst;
    while (rc !== 0xFFFFFFFF && rc < nodeCount) {
      rootChildren.push(tscNodes[rc]);
      tscNodes[rc].parent = sf;
      var nextOff = rc * 24 + 20; // next_sibling offset
      rc = view.getUint32(nextOff, true);
    }
    sf.statements = createNodeArray(rootChildren);

    // EndOfFileToken
    var TokenCtor = ts.objectAllocator.getTokenConstructor();
    sf.endOfFileToken = new TokenCtor(1, sourceText.length, sourceText.length);
    sf.endOfFileToken.parent = sf;

    return sf;
  };
})();
