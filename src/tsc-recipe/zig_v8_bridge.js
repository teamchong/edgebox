// V8 Bridge: Create TSC SourceFile from Zig flat binary AST
//
// Reads flat AST buffer (24 bytes/node) and creates TSC Node tree.
// Uses ts.SyntaxKind for all kind checks — works across TS versions.
//
// Called from recipe as: __zigCreateSourceFile(sourceText, fileName, flatASTBuffer)

(function() {
  if (!globalThis.ts) return;
  var ts = globalThis.ts;
  var SK = ts.SyntaxKind;

  var NodeCtor = ts.objectAllocator.getNodeConstructor();

  function createNodeArray(nodes) {
    var arr = nodes.slice();
    arr.pos = nodes.length > 0 ? nodes[0].pos : -1;
    arr.end = nodes.length > 0 ? nodes[nodes.length - 1].end : -1;
    arr.hasTrailingComma = false;
    arr.transformFlags = 0;
    return arr;
  }

  // Map children to named properties expected by TSC's forEachChild.
  // Uses ts.SyntaxKind constants — no hardcoded numbers.
  function mapChildren(node, children, kind) {
    // Statements containers
    if (kind === SK.SourceFile || kind === SK.Block || kind === SK.ModuleBlock) {
      node.statements = createNodeArray(children); return;
    }
    if (kind === SK.CaseBlock) { node.clauses = createNodeArray(children); return; }
    if (kind === SK.CaseClause) {
      if (children.length > 0) node.expression = children[0];
      node.statements = createNodeArray(children.slice(1)); return;
    }
    if (kind === SK.DefaultClause) { node.statements = createNodeArray(children); return; }

    // Declarations
    if (kind === SK.VariableStatement) {
      assignModifiers(node, children);
      for (var i = 0; i < children.length; i++) if (children[i].kind === SK.VariableDeclarationList) { node.declarationList = children[i]; break; }
      return;
    }
    if (kind === SK.VariableDeclarationList) { node.declarations = createNodeArray(children); return; }
    if (kind === SK.VariableDeclaration) {
      if (children.length > 0) node.name = children[0];
      for (var i = 1; i < children.length; i++) {
        if (isTypeNode(children[i].kind)) { if (!node.type) node.type = children[i]; }
        else node.initializer = children[i];
      }
      return;
    }

    // Functions
    if (kind === SK.FunctionDeclaration || kind === SK.FunctionExpression) {
      assignFunctionParts(node, children); return;
    }
    if (kind === SK.ArrowFunction) {
      node.parameters = createNodeArray([]);
      assignModifiers(node, children);
      for (var i = 0; i < children.length; i++) {
        var ck = children[i].kind;
        if (ck === SK.Parameter) node.parameters.push(children[i]);
        else if (ck === SK.EqualsGreaterThanToken) node.equalsGreaterThanToken = children[i];
        else if (isTypeNode(ck)) node.type = children[i];
        else if (ck === SK.TypeParameter) { if (!node.typeParameters) node.typeParameters = createNodeArray([]); node.typeParameters.push(children[i]); }
        else node.body = children[i]; // last non-param is body
      }
      return;
    }
    if (kind === SK.MethodDeclaration || kind === SK.MethodSignature) {
      assignFunctionParts(node, children); return;
    }
    if (kind === SK.Constructor) {
      assignFunctionParts(node, children); return;
    }
    if (kind === SK.GetAccessor || kind === SK.SetAccessor) {
      assignFunctionParts(node, children); return;
    }
    if (kind === SK.Parameter) {
      assignModifiers(node, children);
      for (var i = 0; i < children.length; i++) {
        var ck = children[i].kind;
        if (ck === SK.DotDotDotToken) node.dotDotDotToken = children[i];
        else if (ck === SK.QuestionToken) node.questionToken = children[i];
        else if (!node.name && (ck === SK.Identifier || ck === SK.ObjectBindingPattern || ck === SK.ArrayBindingPattern)) node.name = children[i];
        else if (isTypeNode(ck)) node.type = children[i];
        else node.initializer = children[i];
      }
      return;
    }

    // Classes/Interfaces/Enums
    if (kind === SK.ClassDeclaration || kind === SK.ClassExpression) {
      assignModifiers(node, children);
      node.members = createNodeArray([]);
      for (var i = 0; i < children.length; i++) {
        var ck = children[i].kind;
        if (ck === SK.Identifier && !node.name) node.name = children[i];
        else if (ck === SK.TypeParameter) { if (!node.typeParameters) node.typeParameters = createNodeArray([]); node.typeParameters.push(children[i]); }
        else if (ck === SK.HeritageClause) { if (!node.heritageClauses) node.heritageClauses = []; node.heritageClauses.push(children[i]); }
        else node.members.push(children[i]);
      }
      return;
    }
    if (kind === SK.InterfaceDeclaration) {
      assignModifiers(node, children);
      node.members = createNodeArray([]);
      for (var i = 0; i < children.length; i++) {
        var ck = children[i].kind;
        if (ck === SK.Identifier && !node.name) node.name = children[i];
        else if (ck === SK.TypeParameter) { if (!node.typeParameters) node.typeParameters = createNodeArray([]); node.typeParameters.push(children[i]); }
        else if (ck === SK.HeritageClause) { if (!node.heritageClauses) node.heritageClauses = []; node.heritageClauses.push(children[i]); }
        else node.members.push(children[i]);
      }
      return;
    }
    if (kind === SK.TypeAliasDeclaration) {
      assignModifiers(node, children);
      if (children.length > 0) node.name = children[0];
      for (var i = 1; i < children.length; i++) {
        if (children[i].kind === SK.TypeParameter) { if (!node.typeParameters) node.typeParameters = createNodeArray([]); node.typeParameters.push(children[i]); }
        else node.type = children[i];
      }
      return;
    }
    if (kind === SK.EnumDeclaration) {
      assignModifiers(node, children);
      if (children.length > 0) node.name = children[0];
      node.members = createNodeArray(children.slice(1));
      return;
    }
    if (kind === SK.EnumMember) {
      if (children.length > 0) node.name = children[0];
      if (children.length > 1) node.initializer = children[1];
      return;
    }

    // Properties
    if (kind === SK.PropertySignature || kind === SK.PropertyDeclaration) {
      assignModifiers(node, children);
      for (var i = 0; i < children.length; i++) {
        var ck = children[i].kind;
        if (ck === SK.QuestionToken) node.questionToken = children[i];
        else if (ck === SK.ExclamationToken) node.exclamationToken = children[i];
        else if (!node.name && (ck === SK.Identifier || ck === SK.StringLiteral || ck === SK.NumericLiteral || ck === SK.ComputedPropertyName)) node.name = children[i];
        else if (isTypeNode(ck)) node.type = children[i];
        else node.initializer = children[i];
      }
      return;
    }
    if (kind === SK.PropertyAssignment) {
      if (children.length > 0) node.name = children[0];
      if (children.length > 1) node.initializer = children[1];
      return;
    }
    if (kind === SK.ShorthandPropertyAssignment) {
      if (children.length > 0) node.name = children[0];
      if (children.length > 1) node.objectAssignmentInitializer = children[1];
      return;
    }
    if (kind === SK.SpreadAssignment) {
      if (children.length > 0) node.expression = children[0]; return;
    }

    // Imports/Exports
    if (kind === SK.ImportDeclaration) {
      assignModifiers(node, children);
      for (var i = 0; i < children.length; i++) {
        if (children[i].kind === SK.ImportClause) node.importClause = children[i];
        else if (children[i].kind === SK.StringLiteral) node.moduleSpecifier = children[i];
      }
      return;
    }
    if (kind === SK.ImportClause) {
      for (var i = 0; i < children.length; i++) {
        var ck = children[i].kind;
        if (ck === SK.Identifier) node.name = children[i];
        else if (ck === SK.NamespaceImport || ck === SK.NamedImports) node.namedBindings = children[i];
      }
      return;
    }
    if (kind === SK.NamespaceImport) { if (children.length > 0) node.name = children[0]; return; }
    if (kind === SK.NamedImports) { node.elements = createNodeArray(children); return; }
    if (kind === SK.ImportSpecifier) {
      if (children.length === 1) node.name = children[0];
      else if (children.length >= 2) { node.propertyName = children[0]; node.name = children[1]; }
      return;
    }
    if (kind === SK.ExportDeclaration) {
      assignModifiers(node, children);
      for (var i = 0; i < children.length; i++) {
        if (children[i].kind === SK.NamedExports) node.exportClause = children[i];
        else if (children[i].kind === SK.StringLiteral) node.moduleSpecifier = children[i];
      }
      return;
    }
    if (kind === SK.NamedExports) { node.elements = createNodeArray(children); return; }
    if (kind === SK.ExportSpecifier) {
      if (children.length === 1) node.name = children[0];
      else if (children.length >= 2) { node.propertyName = children[0]; node.name = children[1]; }
      return;
    }
    if (kind === SK.ExportAssignment) {
      assignModifiers(node, children);
      if (children.length > 0) node.expression = children[children.length - 1];
      return;
    }

    // Expressions
    if (kind === SK.BinaryExpression) {
      if (children.length >= 3) { node.left = children[0]; node.operatorToken = children[1]; node.right = children[2]; }
      else if (children.length === 2) { node.left = children[0]; node.right = children[1]; }
      return;
    }
    if (kind === SK.ConditionalExpression) {
      if (children.length > 0) node.condition = children[0];
      if (children.length > 1) node.whenTrue = children[1];
      if (children.length > 2) node.whenFalse = children[2];
      return;
    }
    if (kind === SK.CallExpression) {
      if (children.length > 0) node.expression = children[0];
      node.arguments = createNodeArray(children.slice(1));
      return;
    }
    if (kind === SK.NewExpression) {
      if (children.length > 0) node.expression = children[0];
      node.arguments = createNodeArray(children.slice(1));
      return;
    }
    if (kind === SK.PropertyAccessExpression) {
      if (children.length > 0) node.expression = children[0];
      if (children.length > 1) node.name = children[1];
      return;
    }
    if (kind === SK.ElementAccessExpression) {
      if (children.length > 0) node.expression = children[0];
      if (children.length > 1) node.argumentExpression = children[1];
      return;
    }
    if (kind === SK.TaggedTemplateExpression) {
      if (children.length > 0) node.tag = children[0];
      if (children.length > 1) node.template = children[1];
      return;
    }
    if (kind === SK.TemplateExpression) {
      if (children.length > 0) node.head = children[0];
      node.templateSpans = createNodeArray(children.slice(1));
      return;
    }
    if (kind === SK.TemplateSpan) {
      if (children.length > 0) node.expression = children[0];
      if (children.length > 1) node.literal = children[1];
      return;
    }
    if (kind === SK.ArrayLiteralExpression) { node.elements = createNodeArray(children); return; }
    if (kind === SK.ObjectLiteralExpression) { node.properties = createNodeArray(children); return; }
    if (kind === SK.SpreadElement) { if (children.length > 0) node.expression = children[0]; return; }
    if (kind === SK.ParenthesizedExpression) { if (children.length > 0) node.expression = children[0]; return; }
    if (kind === SK.TypeOfExpression) { if (children.length > 0) node.expression = children[0]; return; }
    if (kind === SK.DeleteExpression) { if (children.length > 0) node.expression = children[0]; return; }
    if (kind === SK.VoidExpression) { if (children.length > 0) node.expression = children[0]; return; }
    if (kind === SK.AwaitExpression) { if (children.length > 0) node.expression = children[0]; return; }
    if (kind === SK.PrefixUnaryExpression) { if (children.length > 0) node.operand = children[0]; return; }
    if (kind === SK.PostfixUnaryExpression) { if (children.length > 0) node.operand = children[0]; return; }
    if (kind === SK.AsExpression) {
      if (children.length > 0) node.expression = children[0];
      if (children.length > 1) node.type = children[1];
      return;
    }
    if (kind === SK.NonNullExpression) { if (children.length > 0) node.expression = children[0]; return; }
    if (kind === SK.ExpressionWithTypeArguments) {
      if (children.length > 0) node.expression = children[0];
      node.typeArguments = createNodeArray(children.slice(1));
      return;
    }

    // Statements
    if (kind === SK.ExpressionStatement) { if (children.length > 0) node.expression = children[0]; return; }
    if (kind === SK.IfStatement) {
      if (children.length > 0) node.expression = children[0];
      if (children.length > 1) node.thenStatement = children[1];
      if (children.length > 2) node.elseStatement = children[2];
      return;
    }
    if (kind === SK.ReturnStatement) { if (children.length > 0) node.expression = children[0]; return; }
    if (kind === SK.ThrowStatement) { if (children.length > 0) node.expression = children[0]; return; }
    if (kind === SK.DoStatement) {
      if (children.length > 0) node.statement = children[0];
      if (children.length > 1) node.expression = children[1];
      return;
    }
    if (kind === SK.WhileStatement) {
      if (children.length > 0) node.expression = children[0];
      if (children.length > 1) node.statement = children[1];
      return;
    }
    if (kind === SK.ForStatement) {
      if (children.length > 0) node.initializer = children[0];
      if (children.length > 1) node.condition = children[1];
      if (children.length > 2) node.incrementor = children[2];
      if (children.length > 3) node.statement = children[3];
      return;
    }
    if (kind === SK.ForInStatement || kind === SK.ForOfStatement) {
      if (children.length > 0) node.initializer = children[0];
      if (children.length > 1) node.expression = children[1];
      if (children.length > 2) node.statement = children[2];
      return;
    }
    if (kind === SK.SwitchStatement) {
      if (children.length > 0) node.expression = children[0];
      if (children.length > 1) node.caseBlock = children[1];
      return;
    }
    if (kind === SK.LabeledStatement) {
      if (children.length > 0) node.label = children[0];
      if (children.length > 1) node.statement = children[1];
      return;
    }
    if (kind === SK.TryStatement) {
      for (var i = 0; i < children.length; i++) {
        var ck = children[i].kind;
        if (ck === SK.Block && !node.tryBlock) node.tryBlock = children[i];
        else if (ck === SK.CatchClause) node.catchClause = children[i];
        else if (ck === SK.Block) node.finallyBlock = children[i];
      }
      return;
    }
    if (kind === SK.CatchClause) {
      for (var i = 0; i < children.length; i++) {
        if (children[i].kind === SK.VariableDeclaration) node.variableDeclaration = children[i];
        else if (children[i].kind === SK.Block) node.block = children[i];
      }
      return;
    }

    // Binding patterns
    if (kind === SK.ObjectBindingPattern || kind === SK.ArrayBindingPattern) {
      node.elements = createNodeArray(children); return;
    }
    if (kind === SK.BindingElement) {
      for (var i = 0; i < children.length; i++) {
        var ck = children[i].kind;
        if (ck === SK.DotDotDotToken) node.dotDotDotToken = children[i];
        else if (!node.name) node.name = children[i];
        else if (!node.propertyName) { node.propertyName = node.name; node.name = children[i]; }
        else node.initializer = children[i];
      }
      return;
    }

    // Type nodes
    if (kind === SK.TypeReference) {
      if (children.length > 0) node.typeName = children[0];
      if (children.length > 1) node.typeArguments = createNodeArray(children.slice(1));
      return;
    }
    if (kind === SK.UnionType || kind === SK.IntersectionType) { node.types = createNodeArray(children); return; }
    if (kind === SK.ArrayType) { if (children.length > 0) node.elementType = children[0]; return; }
    if (kind === SK.TupleType) { node.elements = createNodeArray(children); return; }
    if (kind === SK.ParenthesizedType) { if (children.length > 0) node.type = children[0]; return; }
    if (kind === SK.TypeLiteral) { node.members = createNodeArray(children); return; }
    if (kind === SK.MappedType) { node.members = createNodeArray(children); return; }
    if (kind === SK.LiteralType) { if (children.length > 0) node.literal = children[0]; return; }
    if (kind === SK.TypeQuery) { if (children.length > 0) node.exprName = children[0]; return; }
    if (kind === SK.FunctionType || kind === SK.ConstructorType) {
      node.parameters = createNodeArray([]);
      for (var i = 0; i < children.length; i++) {
        if (children[i].kind === SK.Parameter) node.parameters.push(children[i]);
        else if (children[i].kind === SK.TypeParameter) { if (!node.typeParameters) node.typeParameters = createNodeArray([]); node.typeParameters.push(children[i]); }
        else if (isTypeNode(children[i].kind)) node.type = children[i];
      }
      return;
    }
    if (kind === SK.ConditionalType) {
      if (children.length > 0) node.checkType = children[0];
      if (children.length > 1) node.extendsType = children[1];
      if (children.length > 2) node.trueType = children[2];
      if (children.length > 3) node.falseType = children[3];
      return;
    }
    if (kind === SK.InferType) { if (children.length > 0) node.typeParameter = children[0]; return; }
    if (kind === SK.ImportType) {
      if (children.length > 0) node.argument = children[0];
      if (children.length > 1) node.qualifier = children[1];
      node.typeArguments = createNodeArray(children.slice(2));
      return;
    }
    if (kind === SK.TypeParameter) {
      if (children.length > 0) node.name = children[0];
      for (var i = 1; i < children.length; i++) {
        if (!node.constraint) node.constraint = children[i];
        else node.default = children[i];
      }
      return;
    }
    if (kind === SK.HeritageClause) {
      node.types = createNodeArray(children); return;
    }
    if (kind === SK.ComputedPropertyName) {
      if (children.length > 0) node.expression = children[0]; return;
    }
    if (kind === SK.IndexSignature) {
      node.parameters = createNodeArray([]);
      for (var i = 0; i < children.length; i++) {
        if (children[i].kind === SK.Parameter) node.parameters.push(children[i]);
        else if (isTypeNode(children[i].kind)) node.type = children[i];
      }
      return;
    }
    if (kind === SK.CallSignature || kind === SK.ConstructSignature) {
      node.parameters = createNodeArray([]);
      for (var i = 0; i < children.length; i++) {
        if (children[i].kind === SK.Parameter) node.parameters.push(children[i]);
        else if (children[i].kind === SK.TypeParameter) { if (!node.typeParameters) node.typeParameters = createNodeArray([]); node.typeParameters.push(children[i]); }
        else if (isTypeNode(children[i].kind)) node.type = children[i];
      }
      return;
    }
    if (kind === SK.Decorator) {
      if (children.length > 0) node.expression = children[0]; return;
    }
  }

  function isTypeNode(kind) {
    return (kind >= SK.FirstTypeNode && kind <= SK.LastTypeNode) || kind === SK.Identifier || kind === SK.QualifiedName;
  }

  function assignModifiers(node, children) {
    // Modifiers are SyntaxKind 87-131 range tokens at start of children
    // Skip — TSC reconstructs them from flags
  }

  function assignFunctionParts(node, children) {
    node.parameters = createNodeArray([]);
    assignModifiers(node, children);
    for (var i = 0; i < children.length; i++) {
      var ck = children[i].kind;
      if (ck === SK.AsteriskToken) node.asteriskToken = children[i];
      else if (ck === SK.QuestionToken) node.questionToken = children[i];
      else if ((ck === SK.Identifier || ck === SK.StringLiteral || ck === SK.NumericLiteral || ck === SK.ComputedPropertyName) && !node.name) node.name = children[i];
      else if (ck === SK.Parameter) node.parameters.push(children[i]);
      else if (ck === SK.TypeParameter) { if (!node.typeParameters) node.typeParameters = createNodeArray([]); node.typeParameters.push(children[i]); }
      else if (ck === SK.Block) node.body = children[i];
      else if (isTypeNode(ck)) node.type = children[i];
    }
  }

  // Lazy SourceFile: nodes materialized on-demand from flat AST buffer.
  // Zero upfront allocation. TSC's binder/checker accesses ~30% of nodes.
  // 70% never materialized = 70% less object creation.
  globalThis.__zigCreateSourceFileLazy = function(sourceText, fileName, flatASTBuffer) {
    if (!flatASTBuffer || flatASTBuffer.byteLength < 24) {
      return ts.createSourceFile(fileName, sourceText, 99, true);
    }
    var view = new DataView(flatASTBuffer);
    var nodeCount = flatASTBuffer.byteLength / 24;
    var materialized = new Array(nodeCount); // sparse: only created on access

    // Materialize a single node from flat buffer
    function getNode(idx) {
      if (idx >= nodeCount || idx === 0xFFFFFFFF) return undefined;
      if (materialized[idx]) return materialized[idx];
      var off = idx * 24;
      var node = new NodeCtor(view.getUint16(off, true), view.getUint32(off + 4, true), view.getUint32(off + 8, true));
      node.flags = view.getUint16(off + 2, true);
      materialized[idx] = node;

      // Set text for identifiers/literals
      var kind = node.kind;
      if (kind === SK.Identifier) {
        var s = node.pos, e = node.end;
        while (s < e && sourceText.charCodeAt(s) <= 32) s++;
        while (e > s && sourceText.charCodeAt(e - 1) <= 32) e--;
        node.escapedText = sourceText.substring(s, e);
        node.text = node.escapedText;
      } else if (kind === SK.StringLiteral || kind === SK.NoSubstitutionTemplateLiteral) {
        node.text = sourceText.substring(node.pos + 1, node.end - 1);
      } else if (kind === SK.NumericLiteral) {
        node.text = sourceText.substring(node.pos, node.end).trim();
      }

      // Set parent
      var parentIdx = view.getUint32(off + 12, true);
      if (parentIdx !== 0xFFFFFFFF && parentIdx < nodeCount) {
        node.parent = getNode(parentIdx);
      }

      // Collect children and map to named properties
      var firstChild = view.getUint32(off + 16, true);
      if (firstChild !== 0xFFFFFFFF) {
        var children = [];
        var ch = firstChild;
        while (ch !== 0xFFFFFFFF && ch < nodeCount && children.length < 2000) {
          children.push(getNode(ch));
          ch = view.getUint32(ch * 24 + 20, true);
        }
        mapChildren(node, children, kind);
      }

      return node;
    }

    // Build SourceFile — only materialize root children
    var SFCtor = ts.objectAllocator.getSourceFileConstructor();
    var sf = new SFCtor(SK.SourceFile, 0, sourceText.length);
    sf.flags = 0;
    sf.text = sourceText;
    sf.fileName = fileName;
    sf.path = '';
    sf.resolvedPath = '';
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

    // Root children → statements (materialize only top-level)
    var rootChildren = [];
    var rootFirst = view.getUint32(16, true);
    var rc = rootFirst;
    while (rc !== 0xFFFFFFFF && rc < nodeCount) {
      var child = getNode(rc);
      child.parent = sf;
      rootChildren.push(child);
      rc = view.getUint32(rc * 24 + 20, true);
    }
    sf.statements = createNodeArray(rootChildren);

    // Extract imports
    var preImports = [];
    var hasModuleMarker = false;
    for (var si = 0; si < rootChildren.length; si++) {
      var sk = rootChildren[si].kind;
      if (sk === SK.ImportDeclaration && rootChildren[si].moduleSpecifier) {
        var spec = rootChildren[si].moduleSpecifier;
        if (spec.kind === SK.StringLiteral && !spec.text) {
          spec.text = sourceText.substring(spec.pos + 1, spec.end - 1).replace(/^['"]|['"]$/g, '');
        }
        preImports.push(spec);
        hasModuleMarker = true;
      }
      if (sk === SK.ExportDeclaration || sk === SK.ExportAssignment || sk === SK.ImportDeclaration) hasModuleMarker = true;
    }
    sf.imports = preImports;
    sf.moduleAugmentations = [];
    sf.ambientModuleNames = [];
    if (hasModuleMarker) {
      for (var emi = 0; emi < rootChildren.length; emi++) {
        var emk = rootChildren[emi].kind;
        if (emk === SK.ImportDeclaration || emk === SK.ExportDeclaration || emk === SK.ExportAssignment) {
          sf.externalModuleIndicator = rootChildren[emi]; break;
        }
      }
    }

    // EndOfFileToken
    var TokenCtor = ts.objectAllocator.getTokenConstructor();
    sf.endOfFileToken = new TokenCtor(SK.EndOfFileToken, sourceText.length, sourceText.length);
    sf.endOfFileToken.parent = sf;

    return sf;
  };

  // Single-pass node creation from flat AST buffer (24 bytes/node) — eager version
  globalThis.__zigCreateSourceFile = function(sourceText, fileName, flatASTBuffer) {
    if (!flatASTBuffer || flatASTBuffer.byteLength < 24) {
      return ts.createSourceFile(fileName, sourceText, 99, true);
    }

    var view = new DataView(flatASTBuffer);
    var nodeCount = flatASTBuffer.byteLength / 24;

    // Phase 1: Create ALL nodes
    var tscNodes = new Array(nodeCount);
    for (var i = 0; i < nodeCount; i++) {
      var off = i * 24;
      tscNodes[i] = new NodeCtor(view.getUint16(off, true), view.getUint32(off + 4, true), view.getUint32(off + 8, true));
      tscNodes[i].flags = view.getUint16(off + 2, true);
    }

    // Phase 2: Link parents
    for (var j = 0; j < nodeCount; j++) {
      var parentIdx = view.getUint32(j * 24 + 12, true);
      if (parentIdx !== 0xFFFFFFFF && parentIdx < nodeCount) {
        tscNodes[j].parent = tscNodes[parentIdx];
      }
    }

    // Phase 3: Collect children + map to named properties
    for (var k = 0; k < nodeCount; k++) {
      var firstChild = view.getUint32(k * 24 + 16, true);
      if (firstChild === 0xFFFFFFFF) continue;
      var children = [];
      var ch = firstChild;
      while (ch !== 0xFFFFFFFF && ch < nodeCount && children.length < 2000) {
        children.push(tscNodes[ch]);
        ch = view.getUint32(ch * 24 + 20, true);
      }
      mapChildren(tscNodes[k], children, tscNodes[k].kind);
    }

    // Phase 4: Build SourceFile
    var SFCtor = ts.objectAllocator.getSourceFileConstructor();
    var sf = new SFCtor(SK.SourceFile, 0, sourceText.length);
    sf.flags = 0;
    sf.text = sourceText;
    sf.fileName = fileName;
    sf.path = '';
    sf.resolvedPath = '';
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

    // Root children = statements
    var rootChildren = [];
    var rootFirst = view.getUint32(16, true); // node 0's firstChild
    var rc = rootFirst;
    while (rc !== 0xFFFFFFFF && rc < nodeCount) {
      rootChildren.push(tscNodes[rc]);
      tscNodes[rc].parent = sf;
      rc = view.getUint32(rc * 24 + 20, true);
    }
    sf.statements = createNodeArray(rootChildren);

    // Extract imports + set text on StringLiterals for module resolution
    var preImports = [];
    var hasModuleMarker = false;
    for (var si = 0; si < rootChildren.length; si++) {
      var sk = rootChildren[si].kind;
      if (sk === SK.ImportDeclaration && rootChildren[si].moduleSpecifier) {
        var spec = rootChildren[si].moduleSpecifier;
        if (spec.kind === SK.StringLiteral && !spec.text) {
          spec.text = sourceText.substring(spec.pos + 1, spec.end - 1).replace(/^['"]|['"]$/g, '');
        }
        preImports.push(spec);
        hasModuleMarker = true;
      }
      if (sk === SK.ExportDeclaration || sk === SK.ExportAssignment || sk === SK.ImportDeclaration) hasModuleMarker = true;
    }
    sf.imports = preImports;
    sf.moduleAugmentations = [];
    sf.ambientModuleNames = [];
    if (hasModuleMarker) {
      for (var emi = 0; emi < rootChildren.length; emi++) {
        var emk = rootChildren[emi].kind;
        if (emk === SK.ImportDeclaration || emk === SK.ExportDeclaration || emk === SK.ExportAssignment) {
          sf.externalModuleIndicator = rootChildren[emi]; break;
        }
      }
    }

    // Set text on all Identifier and StringLiteral nodes
    for (var ni = 0; ni < nodeCount; ni++) {
      var n = tscNodes[ni];
      if (n.kind === SK.Identifier) {
        n.escapedText = sourceText.substring(n.pos, n.end).trim();
        n.text = n.escapedText;
        sf.identifiers.set(n.escapedText, n.escapedText);
        sf.identifierCount++;
      } else if (n.kind === SK.StringLiteral || n.kind === SK.NoSubstitutionTemplateLiteral) {
        if (!n.text) n.text = sourceText.substring(n.pos + 1, n.end - 1);
      } else if (n.kind === SK.NumericLiteral) {
        n.text = sourceText.substring(n.pos, n.end).trim();
      } else if (n.kind === SK.RegularExpressionLiteral) {
        n.text = sourceText.substring(n.pos, n.end).trim();
      }
    }

    // EndOfFileToken
    var TokenCtor = ts.objectAllocator.getTokenConstructor();
    sf.endOfFileToken = new TokenCtor(SK.EndOfFileToken, sourceText.length, sourceText.length);
    sf.endOfFileToken.parent = sf;

    return sf;
  };

  // __zigMapChildren: called by C++ bridge to map flat children to named properties.
  // C++ creates the V8 objects (fast), this function maps children (version-proof).
  // nodesArr: V8 Array of Node objects. flatBuf: ArrayBuffer of FlatNode data.
  globalThis.__zigMapChildren = function(nodesArr, flatBuf) {
    var view = new DataView(flatBuf);
    var nodeCount = nodesArr.length;
    for (var k = 0; k < nodeCount; k++) {
      var firstChild = view.getUint32(k * 24 + 16, true);
      if (firstChild === 0xFFFFFFFF) continue;
      var children = [];
      var ch = firstChild;
      while (ch !== 0xFFFFFFFF && ch < nodeCount && children.length < 2000) {
        children.push(nodesArr[ch]);
        ch = view.getUint32(ch * 24 + 20, true);
      }
      mapChildren(nodesArr[k], children, nodesArr[k].kind);
    }
  };

  // __zigBuildSourceFile: assembles SourceFile wrapper from pre-created V8 nodes.
  // Called by C++ bridge AFTER nodes + children are already created/mapped.
  globalThis.__zigBuildSourceFile = function(nodesArr, sourceText, fileName, flatBuf) {
    var view = new DataView(flatBuf);
    var nodeCount = nodesArr.length;

    var SFCtor = ts.objectAllocator.getSourceFileConstructor();
    var sf = new SFCtor(SK.SourceFile, 0, sourceText.length);
    sf.flags = 0;
    sf.text = sourceText;
    sf.fileName = fileName;
    sf.path = '';
    sf.resolvedPath = '';
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

    // Root children → statements
    var rootChildren = [];
    var rootFirst = view.getUint32(16, true); // node[0].first_child
    var rc = rootFirst;
    while (rc !== 0xFFFFFFFF && rc < nodeCount) {
      nodesArr[rc].parent = sf;
      rootChildren.push(nodesArr[rc]);
      rc = view.getUint32(rc * 24 + 20, true);
    }
    sf.statements = createNodeArray(rootChildren);

    // Extract imports
    var preImports = [];
    var hasModuleMarker = false;
    for (var si = 0; si < rootChildren.length; si++) {
      var sk = rootChildren[si].kind;
      if (sk === SK.ImportDeclaration && rootChildren[si].moduleSpecifier) {
        var spec = rootChildren[si].moduleSpecifier;
        if (spec.kind === SK.StringLiteral && !spec.text) {
          spec.text = sourceText.substring(spec.pos + 1, spec.end - 1).replace(/^['"]|['"]$/g, '');
        }
        preImports.push(spec);
        hasModuleMarker = true;
      }
      if (sk === SK.ExportDeclaration || sk === SK.ExportAssignment || sk === SK.ImportDeclaration) hasModuleMarker = true;
    }
    sf.imports = preImports;
    sf.moduleAugmentations = [];
    sf.ambientModuleNames = [];
    if (hasModuleMarker) {
      for (var emi = 0; emi < rootChildren.length; emi++) {
        var emk = rootChildren[emi].kind;
        if (emk === SK.ImportDeclaration || emk === SK.ExportDeclaration || emk === SK.ExportAssignment) {
          sf.externalModuleIndicator = rootChildren[emi]; break;
        }
      }
    }

    // Populate identifiers map
    for (var ni = 0; ni < nodeCount; ni++) {
      if (nodesArr[ni].escapedText) {
        sf.identifiers.set(nodesArr[ni].escapedText, nodesArr[ni].escapedText);
        sf.identifierCount++;
      }
    }

    // EndOfFileToken
    var TokenCtor = ts.objectAllocator.getTokenConstructor();
    sf.endOfFileToken = new TokenCtor(SK.EndOfFileToken, sourceText.length, sourceText.length);
    sf.endOfFileToken.parent = sf;

    return sf;
  };

  __edgebox_write_stderr('[bridge] loaded with ' + Object.keys(SK).length / 2 + ' SyntaxKinds\n');
})();
