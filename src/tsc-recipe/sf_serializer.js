// SourceFile serializer/deserializer for cross-worker AST sharing.
// Worker 0 serializes parsed SourceFiles to compact binary.
// Workers 1-N deserialize from binary without running TypeScript's parser.
//
// Binary format per SourceFile:
//   [4B nodeCount] [4B childCount] [4B extraCount]
//   Nodes: nodeCount × 16B each: [kind:u16 pos:u32 end:u32 flags:u32 childStart:u16]
//   Children: childCount × 5B each: [propId:u8 nodeIdx:u32]
//   Extras: extraCount × variable: [nodeIdx:u32 textLen:u16 text:...]
//
// Property ID lookup (child property names → u8):
//   0=statements, 1=expression, 2=name, 3=type, 4=body, 5=left, 6=right,
//   7=operatorToken, 8=initializer, 9=parameters, 10=members, 11=modifiers,
//   12=typeParameters, 13=heritageClauses, 14=declarationList, 15=declarations,
//   16=elements, 17=properties, 18=arguments, 19=thenStatement, 20=elseStatement,
//   21=condition, 22=incrementor, 23=statement, 24=block, 25=catchClause,
//   26=finallyBlock, 27=tryBlock, 28=variableDeclaration, 29=typeName,
//   30=typeArguments, 31=constraint, 32=default, 33=dotDotDotToken,
//   34=questionToken, 35=exclamationToken, 36=asteriskToken, 37=endOfFileToken,
//   38=head, 39=templateSpans, 40=tag, 41=comment, 42=literal,
//   43=label, 44=caseBlock, 45=clauses, 46=expression2,
//   47=returnType, 48=equalsGreaterThanToken, 49=importClause,
//   50=moduleSpecifier, 51=exportClause, 52=moduleReference,
//   253=_text, 254=_escapedText, 255=_other
var _propNames = [
  'statements','expression','name','type','body','left','right',
  'operatorToken','initializer','parameters','members','modifiers',
  'typeParameters','heritageClauses','declarationList','declarations',
  'elements','properties','arguments','thenStatement','elseStatement',
  'condition','incrementor','statement','block','catchClause',
  'finallyBlock','tryBlock','variableDeclaration','typeName',
  'typeArguments','constraint','default','dotDotDotToken',
  'questionToken','exclamationToken','asteriskToken','endOfFileToken',
  'head','templateSpans','tag','comment','literal',
  'label','caseBlock','clauses','expression','returnType',
  'equalsGreaterThanToken','importClause','moduleSpecifier','exportClause',
  'moduleReference'
];
var _propNameToId = {};
for (var _pi = 0; _pi < _propNames.length; _pi++) _propNameToId[_propNames[_pi]] = _pi;

// Known child properties for each SyntaxKind (avoids iterating all props).
// Built lazily from first encounter of each kind.
var _kindChildProps = {};

function getChildProps(node) {
  var k = node.kind;
  if (_kindChildProps[k]) return _kindChildProps[k];
  var props = [];
  for (var i = 0; i < _propNames.length; i++) {
    var p = _propNames[i];
    var v = node[p];
    if (v !== undefined && v !== null) {
      if (typeof v === 'object' && (v.kind !== undefined || Array.isArray(v))) {
        props.push(p);
      }
    }
  }
  _kindChildProps[k] = props;
  return props;
}

// Serialize a SourceFile to Uint8Array
globalThis.__serializeSourceFile = function(sf) {
  var nodes = [];
  var children = [];
  var extras = [];
  var nodeMap = new Map(); // node → index

  // BFS to collect all nodes
  var queue = [sf];
  while (queue.length > 0) {
    var node = queue.shift();
    if (!node || typeof node !== 'object' || node.kind === undefined) continue;
    if (nodeMap.has(node)) continue;
    var idx = nodes.length;
    nodeMap.set(node, idx);
    var childStart = children.length;
    nodes.push({ kind: node.kind, pos: node.pos || 0, end: node.end || 0, flags: node.flags || 0, childStart: childStart });

    // Collect text for identifiers/literals
    if (node.escapedText !== undefined && node.escapedText !== '') {
      extras.push({ nodeIdx: idx, text: String(node.escapedText), isProp: 254 });
    } else if (node.text !== undefined && typeof node.text === 'string' && node.text !== '') {
      extras.push({ nodeIdx: idx, text: node.text, isProp: 253 });
    }

    // Collect children
    var childProps = getChildProps(node);
    for (var ci = 0; ci < childProps.length; ci++) {
      var propName = childProps[ci];
      var propId = _propNameToId[propName];
      if (propId === undefined) propId = 255;
      var child = node[propName];
      if (Array.isArray(child)) {
        for (var ai = 0; ai < child.length; ai++) {
          if (child[ai] && child[ai].kind !== undefined) {
            children.push({ propId: propId, node: child[ai], isArray: true });
            queue.push(child[ai]);
          }
        }
      } else if (child && child.kind !== undefined) {
        children.push({ propId: propId, node: child, isArray: false });
        queue.push(child);
      }
    }
    // Update childStart count
    nodes[idx].childEnd = children.length;
  }

  // Encode to binary
  var extraBytes = 0;
  for (var ei = 0; ei < extras.length; ei++) {
    extraBytes += 4 + 2 + extras[ei].text.length * 2; // nodeIdx + textLen + UTF-16
  }
  var totalSize = 12 + nodes.length * 18 + children.length * 6 + extraBytes;
  var buf = new ArrayBuffer(totalSize);
  var view = new DataView(buf);
  var offset = 0;

  // Header
  view.setUint32(offset, nodes.length, true); offset += 4;
  view.setUint32(offset, children.length, true); offset += 4;
  view.setUint32(offset, extras.length, true); offset += 4;

  // Nodes: kind(2) + pos(4) + end(4) + flags(4) + childStart(2) + childEnd(2) = 18B
  for (var ni = 0; ni < nodes.length; ni++) {
    var n = nodes[ni];
    view.setUint16(offset, n.kind, true); offset += 2;
    view.setUint32(offset, n.pos, true); offset += 4;
    view.setUint32(offset, n.end, true); offset += 4;
    view.setUint32(offset, n.flags, true); offset += 4;
    view.setUint16(offset, n.childStart, true); offset += 2;
    view.setUint16(offset, n.childEnd || n.childStart, true); offset += 2;
  }

  // Children: propId(1) + isArray(1) + nodeIdx(4) = 6B
  for (var chi = 0; chi < children.length; chi++) {
    var ch = children[chi];
    view.setUint8(offset, ch.propId); offset += 1;
    view.setUint8(offset, ch.isArray ? 1 : 0); offset += 1;
    // Resolve node index (child.node → nodeMap index)
    var childIdx = nodeMap.get(ch.node);
    view.setUint32(offset, childIdx !== undefined ? childIdx : 0xFFFFFFFF, true); offset += 4;
  }

  // Extras: nodeIdx(4) + textLen(2) + text(UTF-16)
  for (var exi = 0; exi < extras.length; exi++) {
    var ex = extras[exi];
    view.setUint32(offset, ex.nodeIdx, true); offset += 4;
    view.setUint16(offset, ex.text.length, true); offset += 2;
    for (var ti = 0; ti < ex.text.length; ti++) {
      view.setUint16(offset, ex.text.charCodeAt(ti), true); offset += 2;
    }
  }

  return new Uint8Array(buf, 0, offset);
};

// Deserialize a Uint8Array back to a SourceFile-like object
globalThis.__deserializeSourceFile = function(buffer, fileName, text, languageVersion) {
  var view = new DataView(buffer.buffer || buffer, buffer.byteOffset || 0);
  var offset = 0;

  var nodeCount = view.getUint32(offset, true); offset += 4;
  var childCount = view.getUint32(offset, true); offset += 4;
  var extraCount = view.getUint32(offset, true); offset += 4;

  // Read nodes
  var nodeData = new Array(nodeCount);
  for (var ni = 0; ni < nodeCount; ni++) {
    nodeData[ni] = {
      kind: view.getUint16(offset, true),
      pos: view.getUint32(offset + 2, true),
      end: view.getUint32(offset + 6, true),
      flags: view.getUint32(offset + 10, true),
      childStart: view.getUint16(offset + 14, true),
      childEnd: view.getUint16(offset + 16, true),
    };
    offset += 18;
  }

  // Read children
  var childData = new Array(childCount);
  for (var chi = 0; chi < childCount; chi++) {
    childData[chi] = {
      propId: view.getUint8(offset),
      isArray: view.getUint8(offset + 1) === 1,
      nodeIdx: view.getUint32(offset + 2, true),
    };
    offset += 6;
  }

  // Read extras
  var extraData = new Array(extraCount);
  for (var exi = 0; exi < extraCount; exi++) {
    var nodeIdx = view.getUint32(offset, true); offset += 4;
    var textLen = view.getUint16(offset, true); offset += 2;
    var chars = [];
    for (var ti = 0; ti < textLen; ti++) {
      chars.push(String.fromCharCode(view.getUint16(offset, true)));
      offset += 2;
    }
    extraData[exi] = { nodeIdx: nodeIdx, text: chars.join('') };
  }

  // Create node objects (use MonoNode if available, otherwise plain objects)
  var MonoNodeCtor = globalThis.__MonoNodeCtor;
  var nodes = new Array(nodeCount);
  for (var ni = 0; ni < nodeCount; ni++) {
    var nd = nodeData[ni];
    if (MonoNodeCtor) {
      nodes[ni] = new MonoNodeCtor(nd.kind, nd.pos, nd.end);
      nodes[ni].flags = nd.flags;
    } else {
      nodes[ni] = { kind: nd.kind, pos: nd.pos, end: nd.end, flags: nd.flags, id: 0, parent: undefined };
    }
  }

  // Wire children
  for (var ni = 0; ni < nodeCount; ni++) {
    var nd = nodeData[ni];
    var node = nodes[ni];
    // Group children by propId, collect arrays
    var propArrays = {};
    for (var ci = nd.childStart; ci < nd.childEnd; ci++) {
      var ch = childData[ci];
      if (ch.nodeIdx === 0xFFFFFFFF) continue;
      var childNode = nodes[ch.nodeIdx];
      if (!childNode) continue;
      childNode.parent = node;
      var propName = _propNames[ch.propId] || '_unknown';
      if (ch.isArray) {
        if (!propArrays[propName]) propArrays[propName] = [];
        propArrays[propName].push(childNode);
      } else {
        node[propName] = childNode;
      }
    }
    for (var pn in propArrays) {
      node[pn] = propArrays[pn];
    }
  }

  // Apply extras (text, escapedText)
  for (var exi = 0; exi < extraCount; exi++) {
    var ex = extraData[exi];
    var node = nodes[ex.nodeIdx];
    if (node) {
      // For identifiers, set both text and escapedText
      node.text = ex.text;
      node.escapedText = ex.text;
    }
  }

  // Root node is nodes[0] — it's the SourceFile
  var sf = nodes[0];
  sf.fileName = fileName;
  sf.text = text;
  sf.languageVersion = languageVersion || 99;
  sf.languageVariant = 0;
  sf.scriptKind = 3; // TS
  sf.isDeclarationFile = fileName.endsWith('.d.ts');
  sf.hasNoDefaultLib = false;
  sf.identifiers = new Map();
  sf.parseDiagnostics = [];
  sf.bindDiagnostics = [];
  sf.bindSuggestionDiagnostics = [];
  sf.pragmas = new Map();
  sf.referencedFiles = sf.referencedFiles || [];
  sf.typeReferenceDirectives = sf.typeReferenceDirectives || [];
  sf.libReferenceDirectives = sf.libReferenceDirectives || [];
  sf.amdDependencies = sf.amdDependencies || [];
  sf.commentDirectives = undefined;
  sf.nodeCount = nodeCount;
  sf.identifierCount = 0;
  sf.path = fileName;
  sf.resolvedPath = fileName;
  sf.originalFileName = fileName;
  sf.imports = [];
  sf.moduleAugmentations = [];
  sf.ambientModuleNames = [];

  // Build identifiers map and count
  for (var ni = 0; ni < nodeCount; ni++) {
    if (nodes[ni].escapedText) {
      sf.identifiers.set(nodes[ni].escapedText, nodes[ni].escapedText);
      sf.identifierCount++;
    }
  }

  // Extract imports (StringLiteral children of ImportDeclaration/ExportDeclaration)
  // SyntaxKind.ImportDeclaration = 272, ExportDeclaration = 278, ImportEqualsDeclaration = 271
  // SyntaxKind.StringLiteral = 11
  for (var ni = 0; ni < nodeCount; ni++) {
    var n = nodes[ni];
    if (n.moduleSpecifier && n.moduleSpecifier.kind === 11) {
      sf.imports.push(n.moduleSpecifier);
      if (!n.moduleSpecifier.text) {
        n.moduleSpecifier.text = text.substring(n.moduleSpecifier.pos + 1, n.moduleSpecifier.end - 1).trim();
        if (n.moduleSpecifier.text.charAt(0) === '"' || n.moduleSpecifier.text.charAt(0) === "'") {
          n.moduleSpecifier.text = n.moduleSpecifier.text.slice(1, -1);
        }
      }
    }
  }

  return sf;
};
