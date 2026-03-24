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

  // Create a TSC-compatible node from flat AST
  // Each node gets: kind, pos, end, flags, parent, and named child properties
  function createTSCNode(view, nodeCount, sourceText, idx, parentNode) {
    var flat = readNode(view, idx);
    var node = Object.create(ts.objectAllocator.getNodeConstructor().prototype);
    node.kind = flat.kind;
    node.pos = flat.start;
    node.end = flat.end;
    node.flags = flat.flags;
    node.id = 0;
    node.modifierFlagsCache = 0;
    node.transformFlags = 0;
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

  // Main entry: create a SourceFile from Zig-parsed flat AST
  globalThis.__zigCreateSourceFile = function(sourceText, fileName, flatASTBuffer) {
    if (!flatASTBuffer || flatASTBuffer.byteLength < 24) {
      // Fallback to TSC parser
      return ts.createSourceFile(fileName, sourceText, 99, true);
    }

    var view = new DataView(flatASTBuffer);
    var nodeCount = flatASTBuffer.byteLength / 24;

    // Root node should be SourceFile (kind 316)
    var rootFlat = readNode(view, 0);
    if (rootFlat.kind !== 316) {
      return ts.createSourceFile(fileName, sourceText, 99, true);
    }

    // Create SourceFile node
    var sf = Object.create(ts.objectAllocator.getSourceFileConstructor().prototype);
    sf.kind = 316;
    sf.pos = 0;
    sf.end = sourceText.length;
    sf.flags = 0;
    sf.id = 0;
    sf.modifierFlagsCache = 0;
    sf.transformFlags = 0;
    sf.text = sourceText;
    sf.fileName = fileName;
    sf.path = fileName.toLowerCase();
    sf.resolvedPath = fileName.toLowerCase();
    sf.originalFileName = fileName;
    sf.languageVersion = 99;
    sf.languageVariant = 0;
    sf.scriptKind = 3; // TS
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

    // Create statement nodes from flat AST children
    var children = getChildren(view, nodeCount, 0);
    var statements = [];
    for (var i = 0; i < children.length; i++) {
      var stmt = createTSCNode(view, nodeCount, sourceText, children[i], sf);
      statements.push(stmt);
    }
    sf.statements = createNodeArray(statements);

    // EndOfFileToken
    sf.endOfFileToken = Object.create(ts.objectAllocator.getTokenConstructor().prototype);
    sf.endOfFileToken.kind = 1; // EndOfFileToken
    sf.endOfFileToken.pos = sourceText.length;
    sf.endOfFileToken.end = sourceText.length;
    sf.endOfFileToken.flags = 0;
    sf.endOfFileToken.parent = sf;

    // lineMap (required for diagnostic positions)
    sf.lineMap = undefined; // Computed lazily by TSC

    return sf;
  };
})();
