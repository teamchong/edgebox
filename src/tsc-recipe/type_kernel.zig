// TSC Type Relation WASM Kernel
// Compiled to WASM, loaded into V8, inlined by TurboFan.
// Pure flag-based type relation checks — no object access.
//
// Returns: 1 = definitely related (return true)
//          2 = definitely NOT related (return false — skip JS entirely)
//          0 = inconclusive (fall through to JS)

// TSC TypeFlags
const Any: u32 = 1;
const Unknown: u32 = 2;
const String: u32 = 4;
const Number: u32 = 8;
const Boolean: u32 = 16;
const Enum: u32 = 32;
const BigInt: u32 = 64;
const StringLiteral: u32 = 128;
const NumberLiteral: u32 = 256;
const BooleanLiteral: u32 = 512;
const EnumLiteral: u32 = 1024;
const BigIntLiteral: u32 = 2048;
const ESSymbol: u32 = 4096;
const UniqueESSymbol: u32 = 8192;
const Void: u32 = 16384;
const Undefined: u32 = 32768;
const Null: u32 = 65536;
const Never: u32 = 131072;
const TypeParameter: u32 = 262144;
const Object: u32 = 524288;
const Union: u32 = 1048576;
const Intersection: u32 = 2097152;
const Index: u32 = 4194304;
const IndexedAccess: u32 = 8388608;
const Conditional: u32 = 16777216;
const Substitution: u32 = 33554432;
const NonPrimitive: u32 = 67108864;
const TemplateLiteral: u32 = 134217728;
const StringMapping: u32 = 268435456;

// Compound flags
const StringLike: u32 = 402653316; // String | StringLiteral | TemplateLiteral | StringMapping
const NumberLike: u32 = 296; // Number | NumberLiteral | Enum
const BigIntLike: u32 = 2112; // BigInt | BigIntLiteral
const BooleanLike: u32 = 528; // Boolean | BooleanLiteral
const ESSymbolLike: u32 = 12288; // ESSymbol | UniqueESSymbol
const UnionOrIntersection: u32 = 3145728;
const StructuredOrInstantiable: u32 = 469499904; // Object | Union | Intersection | ...
const Primitive: u32 = 402784380; // String | Number | BigInt | Boolean | Enum | ESSymbol | Void | Undefined | Null | Never | Literal types
const Singleton: u32 = 67358815; // Any | Unknown | String | Number | Boolean | BigInt | Void | Undefined | Null | Never | NonPrimitive

// Relation types
const REL_ASSIGNABLE: u32 = 0;
const REL_COMPARABLE: u32 = 1;
const REL_STRICT_SUBTYPE: u32 = 2;

// Returns: 1 = related, 2 = not related, 0 = inconclusive
export fn isSimpleTypeRelated(s: u32, t: u32, rel: u32, strict_null: u32) u32 {
    // ── Definite TRUE cases ──

    // target is Any, or source is Never → always related
    if (t & Any != 0 or s & Never != 0) return 1;
    // target is Unknown (unless strictSubtype + source is Any)
    if (t & Unknown != 0 and !(rel == REL_STRICT_SUBTYPE and s & Any != 0)) return 1;
    // target is Never → never related (Nothing assignable to Never except Never itself)
    if (t & Never != 0) return 2;
    // StringLike → String
    if (s & StringLike != 0 and t & String != 0) return 1;
    // NumberLike → Number
    if (s & NumberLike != 0 and t & Number != 0) return 1;
    // BigIntLike → BigInt
    if (s & BigIntLike != 0 and t & BigInt != 0) return 1;
    // BooleanLike → Boolean
    if (s & BooleanLike != 0 and t & Boolean != 0) return 1;
    // ESSymbolLike → ESSymbol
    if (s & ESSymbolLike != 0 and t & ESSymbol != 0) return 1;
    // Undefined → Undefined|Void (or anything if not strictNullChecks)
    if (s & Undefined != 0) {
        if (strict_null == 0 or t & (Undefined | Void) != 0) return 1;
    }
    // Null → Null (or anything if not strictNullChecks)
    if (s & Null != 0) {
        if (strict_null == 0 or t & Null != 0) return 1;
    }
    // Object → NonPrimitive
    if (s & Object != 0 and t & NonPrimitive != 0) return 1;
    // Assignable/Comparable: Any source → true
    if (rel == REL_ASSIGNABLE or rel == REL_COMPARABLE) {
        if (s & Any != 0) return 1;
        // Number → Enum
        if (s & Number != 0 and t & Enum != 0) return 1;
    }

    // ── Definite FALSE cases (flag combos that can NEVER match) ──
    // These skip TSC's JS checks entirely — biggest performance win.

    // Skip structured/instantiable types — need full JS check
    if (s & StructuredOrInstantiable != 0 or t & StructuredOrInstantiable != 0) {
        // Except: Object → primitive target is always false
        if (s & Object != 0 and t & Object == 0 and t & NonPrimitive == 0 and
            t & Any == 0 and t & Unknown == 0 and t & UnionOrIntersection == 0)
        {
            // Object cannot be assigned to a primitive (String, Number, Boolean, etc.)
            return 2;
        }
        return 0; // Need structural check
    }

    // Both are primitives — if we got here, none of the TRUE rules matched
    // For non-literal primitives, this means they're different base types
    if (s & (String | Number | Boolean | BigInt | ESSymbol | Void | Undefined | Null | Never) != 0 and
        t & (String | Number | Boolean | BigInt | ESSymbol | Void | Undefined | Null | Never) != 0)
    {
        // Different primitive types can't be related (e.g., String vs Number)
        // Exception: in comparable relation, some cross-type comparisons are allowed
        if (rel != REL_COMPARABLE) return 2;
    }

    // StringLiteral → non-String target (excluding Any/Unknown already handled above)
    if (s & StringLiteral != 0 and t & (String | Any | Unknown | StringLiteral) == 0) {
        if (t & UnionOrIntersection == 0) return 2;
    }

    // NumberLiteral → non-Number target
    if (s & NumberLiteral != 0 and t & (Number | Any | Unknown | NumberLiteral | Enum | EnumLiteral) == 0) {
        if (t & UnionOrIntersection == 0) return 2;
    }

    return 0; // Inconclusive — fall through to JS
}
