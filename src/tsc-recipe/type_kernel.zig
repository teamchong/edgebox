// TSC Type Relation WASM Kernel
// Compiled to WASM, loaded into V8, inlined by TurboFan.
// Pure flag-based type relation checks — no object access.

// TSC TypeFlags
const Any: u32 = 1;
const Unknown: u32 = 2;
const String: u32 = 4;
const Number: u32 = 8;
const Boolean: u32 = 16;
const BigInt: u32 = 64;
const StringLiteral: u32 = 128;
const NumberLiteral: u32 = 256;
const BooleanLiteral: u32 = 512;
const EnumLiteral: u32 = 1024;
const BigIntLiteral: u32 = 2048;
const ESSymbol: u32 = 4096;
const Void: u32 = 16384;
const Undefined: u32 = 32768;
const Null: u32 = 65536;
const Never: u32 = 131072;
const Object: u32 = 524288;
const Enum: u32 = 32;
const NonPrimitive: u32 = 67108864;
const StringLike: u32 = 402653316;
const NumberLike: u32 = 296;
const BigIntLike: u32 = 2112;
const BooleanLike: u32 = 528;
const ESSymbolLike: u32 = 12288;

// Relation types
const REL_ASSIGNABLE: u32 = 0;
const REL_COMPARABLE: u32 = 1;
const REL_STRICT_SUBTYPE: u32 = 2;

// Returns: 1 = related, 0 = unknown (needs JS fallback)
export fn isSimpleTypeRelated(s: u32, t: u32, rel: u32, strict_null: u32) u32 {
    if (t & Any != 0 or s & Never != 0) return 1;
    if (t & Unknown != 0 and !(rel == REL_STRICT_SUBTYPE and s & Any != 0)) return 1;
    if (t & Never != 0) return 0;
    if (s & StringLike != 0 and t & String != 0) return 1;
    if (s & NumberLike != 0 and t & Number != 0) return 1;
    if (s & BigIntLike != 0 and t & BigInt != 0) return 1;
    if (s & BooleanLike != 0 and t & Boolean != 0) return 1;
    if (s & ESSymbolLike != 0 and t & ESSymbol != 0) return 1;
    if (s & Undefined != 0) {
        if (strict_null == 0 or t & (Undefined | Void) != 0) return 1;
    }
    if (s & Null != 0) {
        if (strict_null == 0 or t & Null != 0) return 1;
    }
    if (s & Object != 0 and t & NonPrimitive != 0) return 1;
    if (rel == REL_ASSIGNABLE or rel == REL_COMPARABLE) {
        if (s & Any != 0) return 1;
        if (s & Number != 0 and t & Enum != 0) return 1;
    }
    return 0;
}
