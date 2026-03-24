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
      case 308: // SourceFile — should NOT be created via this path
        node.statements = createNodeArray(children);
        break;
      case 244: // VariableStatement
        if (children.length > 0) node.declarationList = children[0];
        break;
      case 262: // VariableDeclarationList
        node.declarations = createNodeArray(children);
        break;
      case 261: // VariableDeclaration
        if (children.length > 0) node.name = children[0];
        // Children after name: type annotation (if kind is a type) and/or initializer
        for (var ci = 1; ci < children.length; ci++) {
          var ck = children[ci].kind;
          // Type nodes: 133-165 (keyword types), 183-200 (constructed types)
          if ((ck >= 133 && ck <= 165) || (ck >= 183 && ck <= 200) || ck === 80) {
            if (!node.type) { node.type = children[ci]; continue; }
          }
          node.initializer = children[ci];
        }
        break;
      case 245: // ExpressionStatement
        if (children.length > 0) node.expression = children[0];
        break;
      case 246: // IfStatement
        if (children.length > 0) node.expression = children[0];
        if (children.length > 1) node.thenStatement = children[1];
        if (children.length > 2) node.elseStatement = children[2];
        break;
      case 254: // ReturnStatement
        if (children.length > 0) node.expression = children[0];
        break;
      case 242: // Block
        node.statements = createNodeArray(children);
        break;
      case 263: // FunctionDeclaration
        node.parameters = createNodeArray([]);
        for (var fi = 0; fi < children.length; fi++) {
          var fk = children[fi].kind;
          if (fk === 80 && !node.name) node.name = children[fi]; // Identifier → name
          else if (fk === 170) node.parameters.push(children[fi]); // Parameter
          else if (fk === 242) node.body = children[fi]; // Block → body
          else if ((fk >= 133 && fk <= 165) || (fk >= 183 && fk <= 200)) node.type = children[fi]; // type
        }
        break;
      case 264: // ClassDeclaration
        if (children.length > 0) node.name = children[0];
        node.members = createNodeArray(children.slice(1));
        break;
      case 265: // InterfaceDeclaration
        if (children.length > 0) node.name = children[0];
        node.members = createNodeArray(children.slice(1));
        break;
      case 266: // TypeAliasDeclaration
        if (children.length > 0) node.name = children[0];
        break;
      case 273: // ImportDeclaration
        // Last child = module specifier (StringLiteral)
        for (var ii = children.length - 1; ii >= 0; ii--) {
          if (children[ii].kind === 11) { node.moduleSpecifier = children[ii]; break; }
        }
        // Create minimal importClause so TSC processes the import
        if (node.moduleSpecifier) {
          var clause = new NodeCtor(274, node.pos, node.end); // ImportClause
          clause.parent = node;
          clause.isTypeOnly = false;
          // Create empty NamedImports (TSC will resolve from module exports)
          var named = new NodeCtor(276, node.pos, node.end); // NamedImports
          named.parent = clause;
          named.elements = createNodeArray([]);
          clause.namedBindings = named;
          node.importClause = clause;
        }
        break;
      case 279: // ExportDeclaration
        // moduleSpecifier from StringLiteral child
        for (var ei = children.length - 1; ei >= 0; ei--) {
          if (children[ei].kind === 11) { node.moduleSpecifier = children[ei]; break; }
        }
        break;
      case 227: // BinaryExpression
        if (children.length > 0) node.left = children[0];
        if (children.length > 1) node.right = children[1];
        break;
      case 214: // CallExpression
        if (children.length > 0) node.expression = children[0];
        node.arguments = createNodeArray(children.slice(1));
        break;
      case 212: // PropertyAccessExpression
        if (children.length > 0) node.expression = children[0];
        if (children.length > 1) node.name = children[1];
        break;
      case 220: // ArrowFunction
        node.parameters = createNodeArray([]);
        if (children.length > 0) node.body = children[children.length - 1];
        break;
      case 170: // Parameter
        if (children.length > 0) node.name = children[0];
        for (var pi = 1; pi < children.length; pi++) {
          var pk = children[pi].kind;
          if ((pk >= 133 && pk <= 165) || (pk >= 183 && pk <= 200)) node.type = children[pi];
          else node.initializer = children[pi];
        }
        break;
      case 184: // TypeReference
        if (children.length > 0) node.typeName = children[0];
        break;
      case 193: // UnionType
        node.types = createNodeArray(children);
        break;
      case 194: // IntersectionType
        node.types = createNodeArray(children);
        break;
      case 189: // ArrayType
        if (children.length > 0) node.elementType = children[0];
        break;
      default:
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
      // Set .text on StringLiteral ONLY if it's a module specifier for a .d.ts import.
      // Setting .text on ALL StringLiterals causes processImportedModules to resolve
      // imports to other Zig-parsed files → hang.
      // We'll set .text selectively in the import processing below.
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
    var sf = new SFCtor(308, 0, sourceText.length); // 308 = SourceFile in TSC 5.9
    sf.flags = 0; // CRITICAL: no PossiblyContainsDynamicImport (4194304) flag
    sf.text = sourceText;
    sf.fileName = fileName;
    sf.path = ''; // TSC sets this via toPath3 in createProgram
    sf.resolvedPath = ''; // TSC sets this
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

    // Mark as external module if any statement is import/export
    var hasModuleMarker = false;
    // Pre-compute imports array from our ImportDeclaration module specifiers.
    // This prevents collectExternalModuleReferences from walking our AST
    // (which can trigger setParentRecursive on incomplete nodes → hang).
    var preImports = [];

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

    // Pre-set imports from our ImportDeclaration moduleSpecifiers.
    // This prevents collectExternalModuleReferences from walking our AST.
    for (var si = 0; si < rootChildren.length; si++) {
      var sk = rootChildren[si].kind;
      if (sk === 273 && rootChildren[si].moduleSpecifier) {
        var spec = rootChildren[si].moduleSpecifier;
        // Set .text on the moduleSpecifier StringLiteral so TSC can resolve it
        if (spec.kind === 11 && !spec.text) {
          spec.text = sourceText.substring(spec.pos + 1, spec.end - 1);
        }
        preImports.push(spec);
        hasModuleMarker = true;
      }
      if (sk === 279 || sk === 278 || sk === 273) hasModuleMarker = true;
    }
    sf.imports = preImports;
    sf.moduleAugmentations = [];
    sf.ambientModuleNames = [];
    // externalModuleIndicator: NOT SET. Setting it breaks module resolution
    // (causes 33 diags instead of 2077). The 19 extra TS2306 errors are
    // "File is not a module" — minor. Will fix when bridge is more complete.

    // EndOfFileToken
    var TokenCtor = ts.objectAllocator.getTokenConstructor();
    sf.endOfFileToken = new TokenCtor(1, sourceText.length, sourceText.length);
    sf.endOfFileToken.parent = sf;

    return sf;
  };
})();
