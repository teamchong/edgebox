// Zig Native TypeScript Parser — Foundation
//
// Goal: replace TSC's JS parser (464ms) with native Zig (~100ms).
// This is the ONLY path to beat tsgo on first cold start.
//
// Architecture:
//   1. Zig scans + parses TypeScript source → flat binary AST
//   2. Binary AST stored in SharedArrayBuffer (or mmap'd file)
//   3. V8 lazily materializes AST nodes on access (via WasmGC or C ABI)
//   4. TSC's binder + checker consume the AST as if TSC parsed it
//
// Current status: SKELETON — defines token types and scanner.
// Next steps:
//   - Implement full TypeScript scanner (identifiers, keywords, operators, strings, templates)
//   - Implement recursive descent parser (statements, expressions, types, declarations)
//   - Implement AST serialization to binary format
//   - Bridge to V8 via C ABI (edgebox_parse_file)
//
// Performance target:
//   - TSC parse (V8 JIT): 464ms for 447 files (6.5MB)
//   - Zig parse target: <100ms for same files
//   - Savings: 364ms × 3 workers = 1092ms CPU, ~364ms wall clock
//   - Total with Zig parser: 219ms parse + 567ms check = 786ms ← beats tsgo (750ms)!

const std = @import("std");

// TypeScript SyntaxKind — must match TSC's values exactly for AST compatibility
pub const SyntaxKind = enum(u16) {
    Unknown = 0,
    EndOfFileToken = 1,
    // Trivia
    SingleLineCommentTrivia = 2,
    MultiLineCommentTrivia = 3,
    NewLineTrivia = 4,
    WhitespaceTrivia = 5,
    // Literals
    NumericLiteral = 9,
    BigIntLiteral = 10,
    StringLiteral = 11,
    RegularExpressionLiteral = 14,
    NoSubstitutionTemplateLiteral = 15,
    TemplateHead = 16,
    TemplateMiddle = 17,
    TemplateTail = 18,
    // Identifiers
    Identifier = 80,
    // Keywords
    BreakKeyword = 83,
    CaseKeyword = 84,
    CatchKeyword = 85,
    ClassKeyword = 86,
    ConstKeyword = 87,
    ContinueKeyword = 88,
    DefaultKeyword = 90,
    DeleteKeyword = 91,
    DoKeyword = 92,
    ElseKeyword = 93,
    EnumKeyword = 94,
    ExportKeyword = 95,
    ExtendsKeyword = 96,
    FalseKeyword = 97,
    FinallyKeyword = 98,
    ForKeyword = 99,
    FunctionKeyword = 100,
    IfKeyword = 101,
    ImportKeyword = 102,
    InKeyword = 103,
    InstanceOfKeyword = 104,
    NewKeyword = 105,
    NullKeyword = 106,
    ReturnKeyword = 107,
    SuperKeyword = 108,
    SwitchKeyword = 109,
    ThisKeyword = 110,
    ThrowKeyword = 111,
    TrueKeyword = 112,
    TryKeyword = 113,
    TypeOfKeyword = 114,
    VarKeyword = 115,
    VoidKeyword = 116,
    WhileKeyword = 117,
    WithKeyword = 118,
    // Contextual keywords
    ImplementsKeyword = 119,
    InterfaceKeyword = 120,
    LetKeyword = 121,
    PackageKeyword = 122,
    PrivateKeyword = 123,
    ProtectedKeyword = 124,
    PublicKeyword = 125,
    StaticKeyword = 126,
    YieldKeyword = 127,
    AbstractKeyword = 128,
    AsKeyword = 129,
    AsyncKeyword = 134,
    AwaitKeyword = 135,
    ConstructorKeyword = 137,
    DeclareKeyword = 138,
    GetKeyword = 139,
    IsKeyword = 142,
    ModuleKeyword = 144,
    NamespaceKeyword = 145,
    ReadonlyKeyword = 148,
    RequireKeyword = 149,
    TypeKeyword = 155,
    FromKeyword = 161,
    OfKeyword = 165,
    // Punctuation
    OpenBraceToken = 19,
    CloseBraceToken = 20,
    OpenParenToken = 21,
    CloseParenToken = 22,
    OpenBracketToken = 23,
    CloseBracketToken = 24,
    DotToken = 25,
    DotDotDotToken = 26,
    SemicolonToken = 27,
    CommaToken = 28,
    LessThanToken = 30,
    GreaterThanToken = 32,
    EqualsToken = 64,
    ColonToken = 59,
    QuestionToken = 58,
    AtToken = 60,
    ExclamationToken = 54,
    AmpersandToken = 51,
    BarToken = 52,
    PlusToken = 40,
    MinusToken = 41,
    AsteriskToken = 42,
    SlashToken = 44,
    ArrowFunction = 219,
};

// TODO: Implement scanner, parser, AST serialization, V8 bridge
// This is the foundation for the Zig native TypeScript parser.
