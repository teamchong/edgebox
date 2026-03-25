function __eb_isSimpleTypeRelatedTo_impl(source, target, relation, errorReporter, strictNullChecks, wildcardType, assignableRelation, comparableRelation, strictSubtypeRelation, identityRelation) {
    const s = source.flags;
    const t = target.flags;
    if (t & 1 /* Any */ || s & 131072 /* Never */ || source === wildcardType) return true;
    if (t & 2 /* Unknown */ && !(relation === strictSubtypeRelation && s & 1 /* Any */)) return true;
    if (t & 131072 /* Never */) return false;
    if (s & 402653316 /* StringLike */ && t & 4 /* String */) return true;
    if (s & 128 /* StringLiteral */ && s & 1024 /* EnumLiteral */ && t & 128 /* StringLiteral */ && !(t & 1024 /* EnumLiteral */) && source.value === target.value) return true;
    if (s & 296 /* NumberLike */ && t & 8 /* Number */) return true;
    if (s & 256 /* NumberLiteral */ && s & 1024 /* EnumLiteral */ && t & 256 /* NumberLiteral */ && !(t & 1024 /* EnumLiteral */) && source.value === target.value) return true;
    if (s & 2112 /* BigIntLike */ && t & 64 /* BigInt */) return true;
    if (s & 528 /* BooleanLike */ && t & 16 /* Boolean */) return true;
    if (s & 12288 /* ESSymbolLike */ && t & 4096 /* ESSymbol */) return true;
    if (s & 32 /* Enum */ && t & 32 /* Enum */ && source.symbol.escapedName === target.symbol.escapedName && isEnumTypeRelatedTo(source.symbol, target.symbol, errorReporter)) return true;
    if (s & 1024 /* EnumLiteral */ && t & 1024 /* EnumLiteral */) {
      if (s & 1048576 /* Union */ && t & 1048576 /* Union */ && isEnumTypeRelatedTo(source.symbol, target.symbol, errorReporter)) return true;
      if (s & 2944 /* Literal */ && t & 2944 /* Literal */ && source.value === target.value && isEnumTypeRelatedTo(source.symbol, target.symbol, errorReporter)) return true;
    }
    if (s & 32768 /* Undefined */ && (!strictNullChecks && !(t & 3145728 /* UnionOrIntersection */) || t & (32768 /* Undefined */ | 16384 /* Void */))) return true;
    if (s & 65536 /* Null */ && (!strictNullChecks && !(t & 3145728 /* UnionOrIntersection */) || t & 65536 /* Null */)) return true;
    if (s & 524288 /* Object */ && t & 67108864 /* NonPrimitive */ && !(relation === strictSubtypeRelation && isEmptyAnonymousObjectType(source) && !(getObjectFlags(source) & 8192 /* FreshLiteral */))) return true;
    if (relation === assignableRelation || relation === comparableRelation) {
      if (s & 1 /* Any */) return true;
      if (s & 8 /* Number */ && (t & 32 /* Enum */ || t & 256 /* NumberLiteral */ && t & 1024 /* EnumLiteral */)) return true;
      if (s & 256 /* NumberLiteral */ && !(s & 1024 /* EnumLiteral */) && (t & 32 /* Enum */ || t & 256 /* NumberLiteral */ && t & 1024 /* EnumLiteral */ && source.value === target.value)) return true;
      if (isUnknownLikeUnionType(target)) return true;
    }
    return false;
  }

function __eb_isTypeRelatedTo_impl(source, target, relation, comparableRelation, identityRelation) {
    if (isFreshLiteralType(source)) {
      source = source.regularType;
    }
    if (isFreshLiteralType(target)) {
      target = target.regularType;
    }
    if (source === target) {
      return true;
    }
    if (relation !== identityRelation) {
      if (relation === comparableRelation && !(target.flags & 131072 /* Never */) && isSimpleTypeRelatedTo(target, source, relation) || isSimpleTypeRelatedTo(source, target, relation)) {
        return true;
      }
    } else if (!((source.flags | target.flags) & (3145728 /* UnionOrIntersection */ | 8388608 /* IndexedAccess */ | 16777216 /* Conditional */ | 33554432 /* Substitution */))) {
      if (source.flags !== target.flags) return false;
      if (source.flags & 67358815 /* Singleton */) return true;
    }
    if (source.flags & 524288 /* Object */ && target.flags & 524288 /* Object */) {
      const related = relation.get(getRelationKey(
        source,
        target,
        0 /* None */,
        relation,
        /*ignoreConstraints*/
        false
      ));
      if (related !== void 0) {
        return !!(related & 1 /* Succeeded */);
      }
    }
    if (source.flags & 469499904 /* StructuredOrInstantiable */ || target.flags & 469499904 /* StructuredOrInstantiable */) {
      return checkTypeRelatedTo(
        source,
        target,
        relation,
        /*errorNode*/
        void 0
      );
    }
    return false;
  }

function __eb_checkBinaryLikeExpression_impl(left, operatorToken, right, checkMode, errorNode, ) {
    const operator = operatorToken.kind;
    if (operator === 64 /* EqualsToken */ && (left.kind === 210 /* ObjectLiteralExpression */ || left.kind === 209 /* ArrayLiteralExpression */)) {
      return checkDestructuringAssignment(left, checkExpression(right, checkMode), checkMode, right.kind === 110 /* ThisKeyword */);
    }
    let leftType;
    if (isBinaryLogicalOperator(operator)) {
      leftType = checkTruthinessExpression(left, checkMode);
    } else {
      leftType = checkExpression(left, checkMode);
    }
    const rightType = checkExpression(right, checkMode);
    return checkBinaryLikeExpressionWorker(left, operatorToken, right, leftType, rightType, checkMode, errorNode);
  }

function __eb_resolveStructuredTypeMembers_impl(type, ) {
    if (!type.members) {
      if (type.flags & 524288 /* Object */) {
        if (type.objectFlags & 4 /* Reference */) {
          resolveTypeReferenceMembers(type);
        } else if (type.objectFlags & 3 /* ClassOrInterface */) {
          resolveClassOrInterfaceMembers(type);
        } else if (type.objectFlags & 1024 /* ReverseMapped */) {
          resolveReverseMappedTypeMembers(type);
        } else if (type.objectFlags & 16 /* Anonymous */) {
          resolveAnonymousTypeMembers(type);
        } else if (type.objectFlags & 32 /* Mapped */) {
          resolveMappedTypeMembers(type);
        } else {
          Debug.fail("Unhandled object type " + Debug.formatObjectFlags(type.objectFlags));
        }
      } else if (type.flags & 1048576 /* Union */) {
        resolveUnionTypeMembers(type);
      } else if (type.flags & 2097152 /* Intersection */) {
        resolveIntersectionTypeMembers(type);
      } else {
        Debug.fail("Unhandled type " + Debug.formatTypeFlags(type.flags));
      }
    }
    return type;
  }

function __eb_checkVariableDeclaration_impl(node, ) {
    var _a, _b;
    (_a = tracing) == null ? void 0 : _a.push(tracing.Phase.Check, "checkVariableDeclaration", { kind: node.kind, pos: node.pos, end: node.end, path: node.tracingPath });
    checkGrammarVariableDeclaration(node);
    checkVariableLikeDeclaration(node);
    (_b = tracing) == null ? void 0 : _b.pop();
  }

function __eb_instantiateType_impl(type, mapper, ) {
    return type && mapper ? instantiateTypeWithAlias(
      type,
      mapper,
      /*aliasSymbol*/
      void 0,
      /*aliasTypeArguments*/
      void 0
    ) : type;
  }

function __eb_checkExpression_impl(node, checkMode, forceTuple, currentNode, instantiationCount) {
    var _a, _b;
    (_a = tracing) == null ? void 0 : _a.push(tracing.Phase.Check, "checkExpression", { kind: node.kind, pos: node.pos, end: node.end, path: node.tracingPath });
    const saveCurrentNode = currentNode;
    currentNode = node;
    instantiationCount = 0;
    const uninstantiatedType = checkExpressionWorker(node, checkMode, forceTuple);
    const type = instantiateTypeWithSingleGenericCallSignature(node, uninstantiatedType, checkMode);
    if (isConstEnumObjectType(type)) {
      checkConstEnumAccess(node, type);
    }
    currentNode = saveCurrentNode;
    (_b = tracing) == null ? void 0 : _b.pop();
    return type;
  }

function __eb_checkSourceElementWorker_impl(node, cancellationToken, compilerOptions) {
    if (getNodeCheckFlags(node) & 8388608 /* PartiallyTypeChecked */) {
      return;
    }
    if (canHaveJSDoc(node)) {
      forEach(node.jsDoc, ({ comment, tags }) => {
        checkJSDocCommentWorker(comment);
        forEach(tags, (tag) => {
          checkJSDocCommentWorker(tag.comment);
          if (isInJSFile(node)) {
            checkSourceElement(tag);
          }
        });
      });
    }
    const kind = node.kind;
    if (cancellationToken) {
      switch (kind) {
        case 267 /* ModuleDeclaration */:
        case 263 /* ClassDeclaration */:
        case 264 /* InterfaceDeclaration */:
        case 262 /* FunctionDeclaration */:
          cancellationToken.throwIfCancellationRequested();
      }
    }
    if (kind >= 243 /* FirstStatement */ && kind <= 259 /* LastStatement */ && canHaveFlowNode(node) && node.flowNode && !isReachableFlowNode(node.flowNode)) {
      errorOrSuggestion(compilerOptions.allowUnreachableCode === false, node, Diagnostics.Unreachable_code_detected);
    }
    switch (kind) {
      case 168 /* TypeParameter */:
        return checkTypeParameter(node);
      case 169 /* Parameter */:
        return checkParameter(node);
      case 172 /* PropertyDeclaration */:
        return checkPropertyDeclaration(node);
      case 171 /* PropertySignature */:
        return checkPropertySignature(node);
      case 185 /* ConstructorType */:
      case 184 /* FunctionType */:
      case 179 /* CallSignature */:
      case 180 /* ConstructSignature */:
      case 181 /* IndexSignature */:
        return checkSignatureDeclaration(node);
      case 174 /* MethodDeclaration */:
      case 173 /* MethodSignature */:
        return checkMethodDeclaration(node);
      case 175 /* ClassStaticBlockDeclaration */:
        return checkClassStaticBlockDeclaration(node);
      case 176 /* Constructor */:
        return checkConstructorDeclaration(node);
      case 177 /* GetAccessor */:
      case 178 /* SetAccessor */:
        return checkAccessorDeclaration(node);
      case 183 /* TypeReference */:
        return checkTypeReferenceNode(node);
      case 182 /* TypePredicate */:
        return checkTypePredicate(node);
      case 186 /* TypeQuery */:
        return checkTypeQuery(node);
      case 187 /* TypeLiteral */:
        return checkTypeLiteral(node);
      case 188 /* ArrayType */:
        return checkArrayType(node);
      case 189 /* TupleType */:
        return checkTupleType(node);
      case 192 /* UnionType */:
      case 193 /* IntersectionType */:
        return checkUnionOrIntersectionType(node);
      case 196 /* ParenthesizedType */:
      case 190 /* OptionalType */:
      case 191 /* RestType */:
        return checkSourceElement(node.type);
      case 197 /* ThisType */:
        return checkThisType(node);
      case 198 /* TypeOperator */:
        return checkTypeOperator(node);
      case 194 /* ConditionalType */:
        return checkConditionalType(node);
      case 195 /* InferType */:
        return checkInferType(node);
      case 203 /* TemplateLiteralType */:
        return checkTemplateLiteralType(node);
      case 205 /* ImportType */:
        return checkImportType(node);
      case 202 /* NamedTupleMember */:
        return checkNamedTupleMember(node);
      case 328 /* JSDocAugmentsTag */:
        return checkJSDocAugmentsTag(node);
      case 329 /* JSDocImplementsTag */:
        return checkJSDocImplementsTag(node);
      case 346 /* JSDocTypedefTag */:
      case 338 /* JSDocCallbackTag */:
      case 340 /* JSDocEnumTag */:
        return checkJSDocTypeAliasTag(node);
      case 345 /* JSDocTemplateTag */:
        return checkJSDocTemplateTag(node);
      case 344 /* JSDocTypeTag */:
        return checkJSDocTypeTag(node);
      case 324 /* JSDocLink */:
      case 325 /* JSDocLinkCode */:
      case 326 /* JSDocLinkPlain */:
        return checkJSDocLinkLikeTag(node);
      case 341 /* JSDocParameterTag */:
        return checkJSDocParameterTag(node);
      case 348 /* JSDocPropertyTag */:
        return checkJSDocPropertyTag(node);
      case 317 /* JSDocFunctionType */:
        checkJSDocFunctionType(node);
      // falls through
      case 315 /* JSDocNonNullableType */:
      case 314 /* JSDocNullableType */:
      case 312 /* JSDocAllType */:
      case 313 /* JSDocUnknownType */:
      case 322 /* JSDocTypeLiteral */:
        checkJSDocTypeIsInJsFile(node);
        forEachChild(node, checkSourceElement);
        return;
      case 318 /* JSDocVariadicType */:
        checkJSDocVariadicType(node);
        return;
      case 309 /* JSDocTypeExpression */:
        return checkSourceElement(node.type);
      case 333 /* JSDocPublicTag */:
      case 335 /* JSDocProtectedTag */:
      case 334 /* JSDocPrivateTag */:
        return checkJSDocAccessibilityModifiers(node);
      case 350 /* JSDocSatisfiesTag */:
        return checkJSDocSatisfiesTag(node);
      case 343 /* JSDocThisTag */:
        return checkJSDocThisTag(node);
      case 351 /* JSDocImportTag */:
        return checkJSDocImportTag(node);
      case 199 /* IndexedAccessType */:
        return checkIndexedAccessType(node);
      case 200 /* MappedType */:
        return checkMappedType(node);
      case 262 /* FunctionDeclaration */:
        return checkFunctionDeclaration(node);
      case 241 /* Block */:
      case 268 /* ModuleBlock */:
        return checkBlock(node);
      case 243 /* VariableStatement */:
        return checkVariableStatement(node);
      case 244 /* ExpressionStatement */:
        return checkExpressionStatement(node);
      case 245 /* IfStatement */:
        return checkIfStatement(node);
      case 246 /* DoStatement */:
        return checkDoStatement(node);
      case 247 /* WhileStatement */:
        return checkWhileStatement(node);
      case 248 /* ForStatement */:
        return checkForStatement(node);
      case 249 /* ForInStatement */:
        return checkForInStatement(node);
      case 250 /* ForOfStatement */:
        return checkForOfStatement(node);
      case 251 /* ContinueStatement */:
      case 252 /* BreakStatement */:
        return checkBreakOrContinueStatement(node);
      case 253 /* ReturnStatement */:
        return checkReturnStatement(node);
      case 254 /* WithStatement */:
        return checkWithStatement(node);
      case 255 /* SwitchStatement */:
        return checkSwitchStatement(node);
      case 256 /* LabeledStatement */:
        return checkLabeledStatement(node);
      case 257 /* ThrowStatement */:
        return checkThrowStatement(node);
      case 258 /* TryStatement */:
        return checkTryStatement(node);
      case 260 /* VariableDeclaration */:
        return checkVariableDeclaration(node);
      case 208 /* BindingElement */:
        return checkBindingElement(node);
      case 263 /* ClassDeclaration */:
        return checkClassDeclaration(node);
      case 264 /* InterfaceDeclaration */:
        return checkInterfaceDeclaration(node);
      case 265 /* TypeAliasDeclaration */:
        return checkTypeAliasDeclaration(node);
      case 266 /* EnumDeclaration */:
        return checkEnumDeclaration(node);
      case 306 /* EnumMember */:
        return checkEnumMember(node);
      case 267 /* ModuleDeclaration */:
        return checkModuleDeclaration(node);
      case 272 /* ImportDeclaration */:
        return checkImportDeclaration(node);
      case 271 /* ImportEqualsDeclaration */:
        return checkImportEqualsDeclaration(node);
      case 278 /* ExportDeclaration */:
        return checkExportDeclaration(node);
      case 277 /* ExportAssignment */:
        return checkExportAssignment(node);
      case 242 /* EmptyStatement */:
      case 259 /* DebuggerStatement */:
        checkGrammarStatementInAmbientContext(node);
        return;
      case 282 /* MissingDeclaration */:
        return checkMissingDeclaration(node);
    }
  }

function __eb_getTypeOfExpression_impl(node, flowInvocationCount, flowTypeCache) {
    const quickType = getQuickTypeOfExpression(node);
    if (quickType) {
      return quickType;
    }
    if (node.flags & 268435456 /* TypeCached */ && flowTypeCache) {
      const cachedType = flowTypeCache[getNodeId(node)];
      if (cachedType) {
        return cachedType;
      }
    }
    const startInvocationCount = flowInvocationCount;
    const type = checkExpression(node, 64 /* TypeOnly */);
    if (flowInvocationCount !== startInvocationCount) {
      const cache = flowTypeCache || (flowTypeCache = []);
      cache[getNodeId(node)] = type;
      setNodeFlags(node, node.flags | 268435456 /* TypeCached */);
    }
    return type;
  }

function __eb_getTypeArguments_impl(type, currentNode, errorType) {
    var _a, _b;
    if (!type.resolvedTypeArguments) {
      if (!pushTypeResolution(type, 5 /* ResolvedTypeArguments */)) {
        return concatenate(type.target.outerTypeParameters, (_a = type.target.localTypeParameters) == null ? void 0 : _a.map(() => errorType)) || emptyArray;
      }
      const node = type.node;
      const typeArguments = !node ? emptyArray : node.kind === 183 /* TypeReference */ ? concatenate(type.target.outerTypeParameters, getEffectiveTypeArguments2(node, type.target.localTypeParameters)) : node.kind === 188 /* ArrayType */ ? [getTypeFromTypeNode(node.elementType)] : map(node.elements, getTypeFromTypeNode);
      if (popTypeResolution()) {
        type.resolvedTypeArguments ?? (type.resolvedTypeArguments = type.mapper ? instantiateTypes(typeArguments, type.mapper) : typeArguments);
      } else {
        type.resolvedTypeArguments ?? (type.resolvedTypeArguments = concatenate(type.target.outerTypeParameters, ((_b = type.target.localTypeParameters) == null ? void 0 : _b.map(() => errorType)) || emptyArray));
        error2(
          type.node || currentNode,
          type.target.symbol ? Diagnostics.Type_arguments_for_0_circularly_reference_themselves : Diagnostics.Tuple_type_arguments_circularly_reference_themselves,
          type.target.symbol && symbolToString(type.target.symbol)
        );
      }
    }
    return type.resolvedTypeArguments;
  }

function __eb_getSignaturesOfType_impl(type, kind, globalArrayType, globalReadonlyArrayType) {
    const result = getSignaturesOfStructuredType(getReducedApparentType(type), kind);
    if (kind === 0 /* Call */ && !length(result) && type.flags & 1048576 /* Union */) {
      if (type.arrayFallbackSignatures) {
        return type.arrayFallbackSignatures;
      }
      let memberName;
      if (everyType(type, (t) => {
        var _a;
        return !!((_a = t.symbol) == null ? void 0 : _a.parent) && isArrayOrTupleSymbol(t.symbol.parent) && (!memberName ? (memberName = t.symbol.escapedName, true) : memberName === t.symbol.escapedName);
      })) {
        const arrayArg = mapType(type, (t) => getMappedType((isReadonlyArraySymbol(t.symbol.parent) ? globalReadonlyArrayType : globalArrayType).typeParameters[0], t.mapper));
        const arrayType = createArrayType(arrayArg, someType(type, (t) => isReadonlyArraySymbol(t.symbol.parent)));
        return type.arrayFallbackSignatures = getSignaturesOfType(getTypeOfPropertyOfType(arrayType, memberName), kind);
      }
      type.arrayFallbackSignatures = result;
    }
    return result;
  }

function __eb_createType_impl(flags, Type7, checker, typeCount) {
    var _a;
    const result = new Type29(checker, flags);
    typeCount++;
    result.id = typeCount;
    (_a = tracing) == null ? void 0 : _a.recordType(result);
    return result;
  }

function __eb_checkReturnStatement_impl(node, compilerOptions, strictNullChecks, undefinedType) {
    if (checkGrammarStatementInAmbientContext(node)) {
      return;
    }
    const container = getContainingFunctionOrClassStaticBlock(node);
    if (container && isClassStaticBlockDeclaration(container)) {
      grammarErrorOnFirstToken(node, Diagnostics.A_return_statement_cannot_be_used_inside_a_class_static_block);
      return;
    }
    if (!container) {
      grammarErrorOnFirstToken(node, Diagnostics.A_return_statement_can_only_be_used_within_a_function_body);
      return;
    }
    const signature = getSignatureFromDeclaration(container);
    const returnType = getReturnTypeOfSignature(signature);
    if (strictNullChecks || node.expression || returnType.flags & 131072 /* Never */) {
      const exprType = node.expression ? checkExpressionCached(node.expression) : undefinedType;
      if (container.kind === 178 /* SetAccessor */) {
        if (node.expression) {
          error2(node, Diagnostics.Setters_cannot_return_a_value);
        }
      } else if (container.kind === 176 /* Constructor */) {
        const exprType2 = node.expression ? checkExpressionCached(node.expression) : undefinedType;
        if (node.expression && !checkTypeAssignableToAndOptionallyElaborate(exprType2, returnType, node, node.expression)) {
          error2(node, Diagnostics.Return_type_of_constructor_signature_must_be_assignable_to_the_instance_type_of_the_class);
        }
      } else if (getReturnTypeFromAnnotation(container)) {
        const unwrappedReturnType = unwrapReturnType(returnType, getFunctionFlags(container)) ?? returnType;
        checkReturnExpression(container, unwrappedReturnType, node, node.expression, exprType);
      }
    } else if (container.kind !== 176 /* Constructor */ && compilerOptions.noImplicitReturns && !isUnwrappedReturnTypeUndefinedVoidOrAny(container, returnType)) {
      error2(node, Diagnostics.Not_all_code_paths_return_a_value);
    }
  }

function __eb_checkCallExpression_impl(node, checkMode, anyType, noImplicitAny, resolvingSignature, silentNeverType, voidType) {
    var _a, _b, _c;
    checkGrammarTypeArguments(node, node.typeArguments);
    const signature = getResolvedSignature(
      node,
      /*candidatesOutArray*/
      void 0,
      checkMode
    );
    if (signature === resolvingSignature) {
      return silentNeverType;
    }
    checkDeprecatedSignature(signature, node);
    if (node.expression.kind === 108 /* SuperKeyword */) {
      return voidType;
    }
    if (node.kind === 214 /* NewExpression */) {
      const declaration = signature.declaration;
      if (declaration && declaration.kind !== 176 /* Constructor */ && declaration.kind !== 180 /* ConstructSignature */ && declaration.kind !== 185 /* ConstructorType */ && !(isJSDocSignature(declaration) && ((_b = (_a = getJSDocRoot(declaration)) == null ? void 0 : _a.parent) == null ? void 0 : _b.kind) === 176 /* Constructor */) && !isJSDocConstructSignature(declaration) && !isJSConstructor(declaration)) {
        if (noImplicitAny) {
          error2(node, Diagnostics.new_expression_whose_target_lacks_a_construct_signature_implicitly_has_an_any_type);
        }
        return anyType;
      }
    }
    if (isInJSFile(node) && isCommonJsRequire(node)) {
      return resolveExternalModuleTypeByLiteral(node.arguments[0]);
    }
    const returnType = getReturnTypeOfSignature(signature);
    if (returnType.flags & 12288 /* ESSymbolLike */ && isSymbolOrSymbolForCall(node)) {
      return getESSymbolLikeTypeForNode(walkUpParenthesizedExpressions(node.parent));
    }
    if (node.kind === 213 /* CallExpression */ && !node.questionDotToken && node.parent.kind === 244 /* ExpressionStatement */ && returnType.flags & 16384 /* Void */ && getTypePredicateOfSignature(signature)) {
      if (!isDottedName(node.expression)) {
        error2(node.expression, Diagnostics.Assertions_require_the_call_target_to_be_an_identifier_or_qualified_name);
      } else if (!getEffectsSignature(node)) {
        const diagnostic = error2(node.expression, Diagnostics.Assertions_require_every_name_in_the_call_target_to_be_declared_with_an_explicit_type_annotation);
        getTypeOfDottedName(node.expression, diagnostic);
      }
    }
    if (isInJSFile(node)) {
      const jsSymbol = getSymbolOfExpando(
        node,
        /*allowDeclaration*/
        false
      );
      if ((_c = jsSymbol == null ? void 0 : jsSymbol.exports) == null ? void 0 : _c.size) {
        const jsAssignmentType = createAnonymousType(jsSymbol, jsSymbol.exports, emptyArray, emptyArray, emptyArray);
        jsAssignmentType.objectFlags |= 4096 /* JSLiteral */;
        return getIntersectionType([returnType, jsAssignmentType]);
      }
    }
    return returnType;
  }

function __eb_getResolvedSignature_impl(node, candidatesOutArray, checkMode, flowLoopCount, flowLoopStart, resolutionStart, resolutionTargets, resolvingSignature) {
    const links = getNodeLinks(node);
    const cached = links.resolvedSignature;
    if (cached && cached !== resolvingSignature && !candidatesOutArray) {
      return cached;
    }
    const saveResolutionStart = resolutionStart;
    if (!cached) {
      resolutionStart = resolutionTargets.length;
    }
    links.resolvedSignature = resolvingSignature;
    const result = resolveSignature(node, candidatesOutArray, checkMode || 0 /* Normal */);
    resolutionStart = saveResolutionStart;
    if (result !== resolvingSignature) {
      links.resolvedSignature = flowLoopStart === flowLoopCount ? result : cached;
    }
    return result;
  }

function __eb_getPropertyOfType_impl(type, name, skipObjectFunctionPropertyAugment, includeTypeOnlyMembers, anyFunctionType, globalCallableFunctionType, globalFunctionType, globalNewableFunctionType, globalObjectType) {
    var _a, _b;
    type = getReducedApparentType(type);
    if (type.flags & 524288 /* Object */) {
      const resolved = resolveStructuredTypeMembers(type);
      const symbol = resolved.members.get(name);
      if (symbol && !includeTypeOnlyMembers && ((_a = type.symbol) == null ? void 0 : _a.flags) & 512 /* ValueModule */ && ((_b = getSymbolLinks(type.symbol).typeOnlyExportStarMap) == null ? void 0 : _b.has(name))) {
        return void 0;
      }
      if (symbol && symbolIsValue(symbol, includeTypeOnlyMembers)) {
        return symbol;
      }
      if (skipObjectFunctionPropertyAugment) return void 0;
      const functionType = resolved === anyFunctionType ? globalFunctionType : resolved.callSignatures.length ? globalCallableFunctionType : resolved.constructSignatures.length ? globalNewableFunctionType : void 0;
      if (functionType) {
        const symbol2 = getPropertyOfObjectType(functionType, name);
        if (symbol2) {
          return symbol2;
        }
      }
      return getPropertyOfObjectType(globalObjectType, name);
    }
    if (type.flags & 2097152 /* Intersection */) {
      const prop = getPropertyOfUnionOrIntersectionType(
        type,
        name,
        /*skipObjectFunctionPropertyAugment*/
        true
      );
      if (prop) {
        return prop;
      }
      if (!skipObjectFunctionPropertyAugment) {
        return getPropertyOfUnionOrIntersectionType(type, name, skipObjectFunctionPropertyAugment);
      }
      return void 0;
    }
    if (type.flags & 1048576 /* Union */) {
      return getPropertyOfUnionOrIntersectionType(type, name, skipObjectFunctionPropertyAugment);
    }
    return void 0;
  }

function __eb_chooseOverload_impl(candidates2, relation, isSingleNonGenericCandidate2, signatureHelpTrailingComma2 = false, ) {
      var _a, _b;
      candidatesForArgumentError = void 0;
      candidateForArgumentArityError = void 0;
      candidateForTypeArgumentError = void 0;
      if (isSingleNonGenericCandidate2) {
        const candidate = candidates2[0];
        if (some(typeArguments) || !hasCorrectArity(node, args, candidate, signatureHelpTrailingComma2)) {
          return void 0;
        }
        if (getSignatureApplicabilityError(
          node,
          args,
          candidate,
          relation,
          0 /* Normal */,
          /*reportErrors*/
          false,
          /*containingMessageChain*/
          void 0,
          /*inferenceContext*/
          void 0
        )) {
          candidatesForArgumentError = [candidate];
          return void 0;
        }
        return candidate;
      }
      for (let candidateIndex = 0; candidateIndex < candidates2.length; candidateIndex++) {
        let candidate = candidates2[candidateIndex];
        if (!hasCorrectTypeArgumentArity(candidate, typeArguments) || !hasCorrectArity(node, args, candidate, signatureHelpTrailingComma2)) {
          continue;
        }
        let checkCandidate;
        let inferenceContext;
        if (candidate.typeParameters) {
          const paramLocation = (_b = (_a = candidate.typeParameters[0].symbol.declarations) == null ? void 0 : _a[0]) == null ? void 0 : _b.parent;
          const candidateParameterContext = paramLocation || (candidate.declaration && isConstructorDeclaration(candidate.declaration) ? candidate.declaration.parent : candidate.declaration);
          if (candidateParameterContext && findAncestor(node, (a) => a === candidateParameterContext)) {
            candidate = getImplementationSignature(candidate);
          }
          let typeArgumentTypes;
          if (some(typeArguments)) {
            typeArgumentTypes = checkTypeArguments(
              candidate,
              typeArguments,
              /*reportErrors*/
              false
            );
            if (!typeArgumentTypes) {
              candidateForTypeArgumentError = candidate;
              continue;
            }
          } else {
            inferenceContext = createInferenceContext(
              candidate.typeParameters,
              candidate,
              /*flags*/
              isInJSFile(node) ? 2 /* AnyDefault */ : 0 /* None */
            );
            typeArgumentTypes = instantiateTypes(inferTypeArguments(node, candidate, args, argCheckMode | 8 /* SkipGenericFunctions */, inferenceContext), inferenceContext.nonFixingMapper);
            argCheckMode |= inferenceContext.flags & 4 /* SkippedGenericFunction */ ? 8 /* SkipGenericFunctions */ : 0 /* Normal */;
          }
          checkCandidate = getSignatureInstantiation(candidate, typeArgumentTypes, isInJSFile(candidate.declaration), inferenceContext && inferenceContext.inferredTypeParameters);
          if (getNonArrayRestType(candidate) && !hasCorrectArity(node, args, checkCandidate, signatureHelpTrailingComma2)) {
            candidateForArgumentArityError = checkCandidate;
            continue;
          }
        } else {
          checkCandidate = candidate;
        }
        if (getSignatureApplicabilityError(
          node,
          args,
          checkCandidate,
          relation,
          argCheckMode,
          /*reportErrors*/
          false,
          /*containingMessageChain*/
          void 0,
          inferenceContext
        )) {
          (candidatesForArgumentError || (candidatesForArgumentError = [])).push(checkCandidate);
          continue;
        }
        if (argCheckMode) {
          argCheckMode = 0 /* Normal */;
          if (inferenceContext) {
            const typeArgumentTypes = instantiateTypes(inferTypeArguments(node, candidate, args, argCheckMode, inferenceContext), inferenceContext.mapper);
            checkCandidate = getSignatureInstantiation(candidate, typeArgumentTypes, isInJSFile(candidate.declaration), inferenceContext.inferredTypeParameters);
            if (getNonArrayRestType(candidate) && !hasCorrectArity(node, args, checkCandidate, signatureHelpTrailingComma2)) {
              candidateForArgumentArityError = checkCandidate;
              continue;
            }
          }
          if (getSignatureApplicabilityError(
            node,
            args,
            checkCandidate,
            relation,
            argCheckMode,
            /*reportErrors*/
            false,
            /*containingMessageChain*/
            void 0,
            inferenceContext
          )) {
            (candidatesForArgumentError || (candidatesForArgumentError = [])).push(checkCandidate);
            continue;
          }
        }
        candidates2[candidateIndex] = checkCandidate;
        return checkCandidate;
      }
      return void 0;
    }

function __eb_getApparentType_impl(type, emptyObjectType, globalBooleanType, globalNumberType, globalStringType, strictNullChecks, stringNumberSymbolType, unknownType) {
    const t = type.flags & 465829888 /* Instantiable */ ? getBaseConstraintOfType(type) || unknownType : type;
    const objectFlags = getObjectFlags(t);
    return objectFlags & 32 /* Mapped */ ? getApparentTypeOfMappedType(t) : objectFlags & 4 /* Reference */ && t !== type ? getTypeWithThisArgument(t, type) : t.flags & 2097152 /* Intersection */ ? getApparentTypeOfIntersectionType(t, type) : t.flags & 402653316 /* StringLike */ ? globalStringType : t.flags & 296 /* NumberLike */ ? globalNumberType : t.flags & 2112 /* BigIntLike */ ? getGlobalBigIntType() : t.flags & 528 /* BooleanLike */ ? globalBooleanType : t.flags & 12288 /* ESSymbolLike */ ? getGlobalESSymbolType() : t.flags & 67108864 /* NonPrimitive */ ? emptyObjectType : t.flags & 4194304 /* Index */ ? stringNumberSymbolType : t.flags & 2 /* Unknown */ && !strictNullChecks ? emptyObjectType : t;
  }

var _test = 1;
