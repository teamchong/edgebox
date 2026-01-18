//! QuickJS Opcode Definitions
//! Auto-generated from vendor/quickjs-ng/quickjs-opcode.h
//! 248 permanent opcodes + format definitions

const std = @import("std");

/// Opcode operand format (determines instruction size)
pub const Format = enum(u8) {
    none,           // 1 byte (opcode only)
    none_int,       // 1 byte, implicit int constant
    none_loc,       // 1 byte, implicit local index
    none_arg,       // 1 byte, implicit arg index
    none_var_ref,   // 1 byte, implicit var_ref index
    u8,             // 2 bytes (opcode + u8)
    i8,             // 2 bytes (opcode + i8)
    loc8,           // 2 bytes (opcode + local index u8)
    const8,         // 2 bytes (opcode + const pool index u8)
    label8,         // 2 bytes (opcode + jump offset i8)
    u16,            // 3 bytes (opcode + u16)
    i16,            // 3 bytes (opcode + i16)
    label16,        // 3 bytes (opcode + jump offset i16)
    npop,           // 3 bytes (opcode + argc u16)
    npopx,          // 1 byte, argc implicit in opcode (call0-3)
    npop_u16,       // 3 bytes (opcode + argc u16)
    loc,            // 3 bytes (opcode + local index u16)
    arg,            // 3 bytes (opcode + arg index u16)
    var_ref,        // 3 bytes (opcode + var_ref index u16)
    u32,            // 5 bytes (opcode + u32)
    u32x2,          // 9 bytes (opcode + 2*u32)
    i32,            // 5 bytes (opcode + i32)
    @"const",       // 5 bytes (opcode + const pool index u32)
    label,          // 5 bytes (opcode + jump offset i32)
    atom,           // 5 bytes (opcode + atom index u32)
    atom_u8,        // 6 bytes (opcode + atom + u8)
    atom_u16,       // 7 bytes (opcode + atom + u16)
    atom_label_u8,  // 10 bytes (opcode + atom + label + u8)
    atom_label_u16, // 11 bytes (opcode + atom + label + u16)
    label_u16,      // 7 bytes (opcode + label + u16)
};

/// Get instruction size in bytes from format
pub fn formatSize(fmt: Format) u8 {
    return switch (fmt) {
        .none, .none_int, .none_loc, .none_arg, .none_var_ref, .npopx => 1,
        .u8, .i8, .loc8, .const8, .label8 => 2,
        .u16, .i16, .label16, .npop, .npop_u16, .loc, .arg, .var_ref => 3,
        .u32, .i32, .@"const", .label, .atom => 5,
        .atom_u8 => 6,
        .atom_u16, .label_u16 => 7,
        .u32x2 => 9,
        .atom_label_u8 => 10,
        .atom_label_u16 => 11,
    };
}

/// Opcode categories for code generation strategy
pub const Category = enum {
    simple,         // Direct inline (stack ops, push constants)
    arithmetic,     // Type-specialized with int fast path
    comparison,     // Comparison with int fast path
    control_flow,   // Jumps, calls, returns
    variable,       // Local/arg/closure variable access
    property,       // Object property access
    complex,        // Runtime calls (get_var, call, etc.)
    iterator,       // Iterator operations (for-in/for-of)
    closure,        // Closure variable access (can freeze with native closure support)
    never_freeze,   // Cannot freeze (eval, yield, closures)
};

/// Opcode information
pub const OpcodeInfo = struct {
    name: []const u8,
    size: u8,
    n_pop: i8,    // Stack items popped (-1 = variable)
    n_push: i8,   // Stack items pushed
    format: Format,
    category: Category,
};

/// QuickJS opcodes enum
pub const Opcode = enum(u8) {
    invalid = 0,
    // Push values
    push_i32 = 1,
    push_const = 2,
    fclosure = 3,
    push_atom_value = 4,
    private_symbol = 5,
    undefined = 6,
    null = 7,
    push_this = 8,
    push_false = 9,
    push_true = 10,
    object = 11,
    special_object = 12,
    rest = 13,
    // Stack manipulation
    drop = 14,
    nip = 15,
    nip1 = 16,
    dup = 17,
    dup1 = 18,
    dup2 = 19,
    dup3 = 20,
    insert2 = 21,
    insert3 = 22,
    insert4 = 23,
    perm3 = 24,
    perm4 = 25,
    perm5 = 26,
    swap = 27,
    swap2 = 28,
    rot3l = 29,
    rot3r = 30,
    rot4l = 31,
    rot5l = 32,
    // Calls
    call_constructor = 33,
    call = 34,
    tail_call = 35,
    call_method = 36,
    tail_call_method = 37,
    array_from = 38,
    apply = 39,
    @"return" = 40,
    return_undef = 41,
    check_ctor_return = 42,
    check_ctor = 43,
    init_ctor = 44,
    check_brand = 45,
    add_brand = 46,
    return_async = 47,
    throw = 48,
    throw_error = 49,
    eval = 50,
    apply_eval = 51,
    regexp = 52,
    get_super = 53,
    import = 54,
    // Variables
    check_var = 55,
    get_var_undef = 56,
    get_var = 57,
    put_var = 58,
    put_var_init = 59,
    put_var_strict = 60,
    get_ref_value = 61,
    put_ref_value = 62,
    define_var = 63,
    check_define_var = 64,
    define_func = 65,
    // Properties
    get_field = 66,
    get_field2 = 67,
    put_field = 68,
    get_private_field = 69,
    put_private_field = 70,
    define_private_field = 71,
    get_array_el = 72,
    get_array_el2 = 73,
    put_array_el = 74,
    get_super_value = 75,
    put_super_value = 76,
    define_field = 77,
    set_name = 78,
    set_name_computed = 79,
    set_proto = 80,
    set_home_object = 81,
    define_array_el = 82,
    append = 83,
    copy_data_properties = 84,
    define_method = 85,
    define_method_computed = 86,
    define_class = 87,
    define_class_computed = 88,
    // Local/arg access
    get_loc = 89,
    put_loc = 90,
    set_loc = 91,
    get_arg = 92,
    put_arg = 93,
    set_arg = 94,
    get_var_ref = 95,
    put_var_ref = 96,
    set_var_ref = 97,
    set_loc_uninitialized = 98,
    get_loc_check = 99,
    put_loc_check = 100,
    put_loc_check_init = 101,
    get_var_ref_check = 102,
    put_var_ref_check = 103,
    put_var_ref_check_init = 104,
    close_loc = 105,
    // Control flow
    if_false = 106,
    if_true = 107,
    goto = 108,
    @"catch" = 109,
    gosub = 110,
    ret = 111,
    nip_catch = 112,
    // Type conversion
    to_object = 113,
    to_propkey = 114,
    to_propkey2 = 115,
    // With statement
    with_get_var = 116,
    with_put_var = 117,
    with_delete_var = 118,
    with_make_ref = 119,
    with_get_ref = 120,
    with_get_ref_undef = 121,
    // Make ref
    make_loc_ref = 122,
    make_arg_ref = 123,
    make_var_ref_ref = 124,
    make_var_ref = 125,
    // Iterators
    for_in_start = 126,
    for_of_start = 127,
    for_await_of_start = 128,
    for_in_next = 129,
    for_of_next = 130,
    iterator_check_object = 131,
    iterator_get_value_done = 132,
    iterator_close = 133,
    iterator_next = 134,
    iterator_call = 135,
    initial_yield = 136,
    yield = 137,
    yield_star = 138,
    async_yield_star = 139,
    await = 140,
    // Arithmetic
    neg = 141,
    plus = 142,
    dec = 143,
    inc = 144,
    post_dec = 145,
    post_inc = 146,
    dec_loc = 147,
    inc_loc = 148,
    add_loc = 149,
    not = 150,
    lnot = 151,
    typeof = 152,
    delete = 153,
    delete_var = 154,
    mul = 155,
    div = 156,
    mod = 157,
    add = 158,
    sub = 159,
    shl = 160,
    sar = 161,
    shr = 162,
    @"and" = 163,
    xor = 164,
    @"or" = 165,
    pow = 166,
    // Comparison
    lt = 167,
    lte = 168,
    gt = 169,
    gte = 170,
    instanceof = 171,
    in = 172,
    eq = 173,
    neq = 174,
    strict_eq = 175,
    strict_neq = 176,
    is_undefined_or_null = 177,
    private_in = 178,
    push_bigint_i32 = 179,
    nop = 180,
    // Short forms (optimized)
    push_minus1 = 181,
    push_0 = 182,
    push_1 = 183,
    push_2 = 184,
    push_3 = 185,
    push_4 = 186,
    push_5 = 187,
    push_6 = 188,
    push_7 = 189,
    push_i8 = 190,
    push_i16 = 191,
    push_const8 = 192,
    fclosure8 = 193,
    push_empty_string = 194,
    get_loc8 = 195,
    put_loc8 = 196,
    set_loc8 = 197,
    get_loc0_loc1 = 198,
    get_loc0 = 199,
    get_loc1 = 200,
    get_loc2 = 201,
    get_loc3 = 202,
    put_loc0 = 203,
    put_loc1 = 204,
    put_loc2 = 205,
    put_loc3 = 206,
    set_loc0 = 207,
    set_loc1 = 208,
    set_loc2 = 209,
    set_loc3 = 210,
    get_arg0 = 211,
    get_arg1 = 212,
    get_arg2 = 213,
    get_arg3 = 214,
    put_arg0 = 215,
    put_arg1 = 216,
    put_arg2 = 217,
    put_arg3 = 218,
    set_arg0 = 219,
    set_arg1 = 220,
    set_arg2 = 221,
    set_arg3 = 222,
    get_var_ref0 = 223,
    get_var_ref1 = 224,
    get_var_ref2 = 225,
    get_var_ref3 = 226,
    put_var_ref0 = 227,
    put_var_ref1 = 228,
    put_var_ref2 = 229,
    put_var_ref3 = 230,
    set_var_ref0 = 231,
    set_var_ref1 = 232,
    set_var_ref2 = 233,
    set_var_ref3 = 234,
    get_length = 235,
    if_false8 = 236,
    if_true8 = 237,
    goto8 = 238,
    goto16 = 239,
    call0 = 240,
    call1 = 241,
    call2 = 242,
    call3 = 243,
    is_undefined = 244,
    is_null = 245,
    typeof_is_undefined = 246,
    typeof_is_function = 247,
    _,
};

/// Opcode information table
pub const opcode_info = blk: {
    var info: [256]OpcodeInfo = undefined;

    // Initialize all to invalid
    for (&info) |*i| {
        i.* = .{ .name = "invalid", .size = 1, .n_pop = 0, .n_push = 0, .format = .none, .category = .never_freeze };
    }

    // Push values
    info[@intFromEnum(Opcode.push_i32)] = .{ .name = "push_i32", .size = 5, .n_pop = 0, .n_push = 1, .format = .i32, .category = .simple };
    info[@intFromEnum(Opcode.push_const)] = .{ .name = "push_const", .size = 5, .n_pop = 0, .n_push = 1, .format = .@"const", .category = .simple };
    info[@intFromEnum(Opcode.fclosure)] = .{ .name = "fclosure", .size = 5, .n_pop = 0, .n_push = 1, .format = .@"const", .category = .never_freeze };
    info[@intFromEnum(Opcode.push_atom_value)] = .{ .name = "push_atom_value", .size = 5, .n_pop = 0, .n_push = 1, .format = .atom, .category = .simple };
    info[@intFromEnum(Opcode.private_symbol)] = .{ .name = "private_symbol", .size = 5, .n_pop = 0, .n_push = 1, .format = .atom, .category = .complex };
    info[@intFromEnum(Opcode.undefined)] = .{ .name = "undefined", .size = 1, .n_pop = 0, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.null)] = .{ .name = "null", .size = 1, .n_pop = 0, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.push_this)] = .{ .name = "push_this", .size = 1, .n_pop = 0, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.push_false)] = .{ .name = "push_false", .size = 1, .n_pop = 0, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.push_true)] = .{ .name = "push_true", .size = 1, .n_pop = 0, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.object)] = .{ .name = "object", .size = 1, .n_pop = 0, .n_push = 1, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.special_object)] = .{ .name = "special_object", .size = 2, .n_pop = 0, .n_push = 1, .format = .u8, .category = .complex };
    info[@intFromEnum(Opcode.rest)] = .{ .name = "rest", .size = 3, .n_pop = 0, .n_push = 1, .format = .u16, .category = .complex };

    // Stack manipulation
    info[@intFromEnum(Opcode.drop)] = .{ .name = "drop", .size = 1, .n_pop = 1, .n_push = 0, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.nip)] = .{ .name = "nip", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.nip1)] = .{ .name = "nip1", .size = 1, .n_pop = 3, .n_push = 2, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.dup)] = .{ .name = "dup", .size = 1, .n_pop = 1, .n_push = 2, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.dup1)] = .{ .name = "dup1", .size = 1, .n_pop = 2, .n_push = 3, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.dup2)] = .{ .name = "dup2", .size = 1, .n_pop = 2, .n_push = 4, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.dup3)] = .{ .name = "dup3", .size = 1, .n_pop = 3, .n_push = 6, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.insert2)] = .{ .name = "insert2", .size = 1, .n_pop = 2, .n_push = 3, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.insert3)] = .{ .name = "insert3", .size = 1, .n_pop = 3, .n_push = 4, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.insert4)] = .{ .name = "insert4", .size = 1, .n_pop = 4, .n_push = 5, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.perm3)] = .{ .name = "perm3", .size = 1, .n_pop = 3, .n_push = 3, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.perm4)] = .{ .name = "perm4", .size = 1, .n_pop = 4, .n_push = 4, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.perm5)] = .{ .name = "perm5", .size = 1, .n_pop = 5, .n_push = 5, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.swap)] = .{ .name = "swap", .size = 1, .n_pop = 2, .n_push = 2, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.swap2)] = .{ .name = "swap2", .size = 1, .n_pop = 4, .n_push = 4, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.rot3l)] = .{ .name = "rot3l", .size = 1, .n_pop = 3, .n_push = 3, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.rot3r)] = .{ .name = "rot3r", .size = 1, .n_pop = 3, .n_push = 3, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.rot4l)] = .{ .name = "rot4l", .size = 1, .n_pop = 4, .n_push = 4, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.rot5l)] = .{ .name = "rot5l", .size = 1, .n_pop = 5, .n_push = 5, .format = .none, .category = .simple };

    // Calls
    info[@intFromEnum(Opcode.call_constructor)] = .{ .name = "call_constructor", .size = 3, .n_pop = 2, .n_push = 1, .format = .npop, .category = .control_flow };
    info[@intFromEnum(Opcode.call)] = .{ .name = "call", .size = 3, .n_pop = 1, .n_push = 1, .format = .npop, .category = .control_flow };
    info[@intFromEnum(Opcode.tail_call)] = .{ .name = "tail_call", .size = 3, .n_pop = 1, .n_push = 0, .format = .npop, .category = .control_flow };
    info[@intFromEnum(Opcode.call_method)] = .{ .name = "call_method", .size = 3, .n_pop = 2, .n_push = 1, .format = .npop, .category = .control_flow };
    info[@intFromEnum(Opcode.tail_call_method)] = .{ .name = "tail_call_method", .size = 3, .n_pop = 2, .n_push = 0, .format = .npop, .category = .control_flow };
    info[@intFromEnum(Opcode.array_from)] = .{ .name = "array_from", .size = 3, .n_pop = 0, .n_push = 1, .format = .npop, .category = .complex };
    info[@intFromEnum(Opcode.apply)] = .{ .name = "apply", .size = 3, .n_pop = 3, .n_push = 1, .format = .u16, .category = .complex };
    info[@intFromEnum(Opcode.@"return")] = .{ .name = "return", .size = 1, .n_pop = 1, .n_push = 0, .format = .none, .category = .control_flow };
    info[@intFromEnum(Opcode.return_undef)] = .{ .name = "return_undef", .size = 1, .n_pop = 0, .n_push = 0, .format = .none, .category = .control_flow };
    info[@intFromEnum(Opcode.check_ctor_return)] = .{ .name = "check_ctor_return", .size = 1, .n_pop = 1, .n_push = 2, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.check_ctor)] = .{ .name = "check_ctor", .size = 1, .n_pop = 0, .n_push = 0, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.init_ctor)] = .{ .name = "init_ctor", .size = 1, .n_pop = 0, .n_push = 1, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.check_brand)] = .{ .name = "check_brand", .size = 1, .n_pop = 2, .n_push = 2, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.add_brand)] = .{ .name = "add_brand", .size = 1, .n_pop = 2, .n_push = 0, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.return_async)] = .{ .name = "return_async", .size = 1, .n_pop = 1, .n_push = 0, .format = .none, .category = .never_freeze };
    info[@intFromEnum(Opcode.throw)] = .{ .name = "throw", .size = 1, .n_pop = 1, .n_push = 0, .format = .none, .category = .control_flow };
    info[@intFromEnum(Opcode.throw_error)] = .{ .name = "throw_error", .size = 6, .n_pop = 0, .n_push = 0, .format = .atom_u8, .category = .control_flow };
    info[@intFromEnum(Opcode.eval)] = .{ .name = "eval", .size = 5, .n_pop = 1, .n_push = 1, .format = .npop_u16, .category = .never_freeze };
    info[@intFromEnum(Opcode.apply_eval)] = .{ .name = "apply_eval", .size = 3, .n_pop = 2, .n_push = 1, .format = .u16, .category = .never_freeze };
    info[@intFromEnum(Opcode.regexp)] = .{ .name = "regexp", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.get_super)] = .{ .name = "get_super", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.import)] = .{ .name = "import", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .complex };

    // Variables
    info[@intFromEnum(Opcode.check_var)] = .{ .name = "check_var", .size = 5, .n_pop = 0, .n_push = 1, .format = .atom, .category = .complex };
    info[@intFromEnum(Opcode.get_var_undef)] = .{ .name = "get_var_undef", .size = 5, .n_pop = 0, .n_push = 1, .format = .atom, .category = .complex };
    info[@intFromEnum(Opcode.get_var)] = .{ .name = "get_var", .size = 5, .n_pop = 0, .n_push = 1, .format = .atom, .category = .complex };
    info[@intFromEnum(Opcode.put_var)] = .{ .name = "put_var", .size = 5, .n_pop = 1, .n_push = 0, .format = .atom, .category = .complex };
    info[@intFromEnum(Opcode.put_var_init)] = .{ .name = "put_var_init", .size = 5, .n_pop = 1, .n_push = 0, .format = .atom, .category = .complex };
    info[@intFromEnum(Opcode.put_var_strict)] = .{ .name = "put_var_strict", .size = 5, .n_pop = 2, .n_push = 0, .format = .atom, .category = .complex };
    info[@intFromEnum(Opcode.get_ref_value)] = .{ .name = "get_ref_value", .size = 1, .n_pop = 2, .n_push = 3, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.put_ref_value)] = .{ .name = "put_ref_value", .size = 1, .n_pop = 3, .n_push = 0, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.define_var)] = .{ .name = "define_var", .size = 6, .n_pop = 0, .n_push = 0, .format = .atom_u8, .category = .complex };
    info[@intFromEnum(Opcode.check_define_var)] = .{ .name = "check_define_var", .size = 6, .n_pop = 0, .n_push = 0, .format = .atom_u8, .category = .complex };
    info[@intFromEnum(Opcode.define_func)] = .{ .name = "define_func", .size = 6, .n_pop = 1, .n_push = 0, .format = .atom_u8, .category = .complex };

    // Properties
    info[@intFromEnum(Opcode.get_field)] = .{ .name = "get_field", .size = 5, .n_pop = 1, .n_push = 1, .format = .atom, .category = .property };
    info[@intFromEnum(Opcode.get_field2)] = .{ .name = "get_field2", .size = 5, .n_pop = 1, .n_push = 2, .format = .atom, .category = .property };
    info[@intFromEnum(Opcode.put_field)] = .{ .name = "put_field", .size = 5, .n_pop = 2, .n_push = 0, .format = .atom, .category = .property };
    info[@intFromEnum(Opcode.get_private_field)] = .{ .name = "get_private_field", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.put_private_field)] = .{ .name = "put_private_field", .size = 1, .n_pop = 3, .n_push = 0, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.define_private_field)] = .{ .name = "define_private_field", .size = 1, .n_pop = 3, .n_push = 1, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.get_array_el)] = .{ .name = "get_array_el", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.get_array_el2)] = .{ .name = "get_array_el2", .size = 1, .n_pop = 2, .n_push = 2, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.put_array_el)] = .{ .name = "put_array_el", .size = 1, .n_pop = 3, .n_push = 0, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.get_super_value)] = .{ .name = "get_super_value", .size = 1, .n_pop = 3, .n_push = 1, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.put_super_value)] = .{ .name = "put_super_value", .size = 1, .n_pop = 4, .n_push = 0, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.define_field)] = .{ .name = "define_field", .size = 5, .n_pop = 2, .n_push = 1, .format = .atom, .category = .property };
    info[@intFromEnum(Opcode.set_name)] = .{ .name = "set_name", .size = 5, .n_pop = 1, .n_push = 1, .format = .atom, .category = .property };
    info[@intFromEnum(Opcode.set_name_computed)] = .{ .name = "set_name_computed", .size = 1, .n_pop = 2, .n_push = 2, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.set_proto)] = .{ .name = "set_proto", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.set_home_object)] = .{ .name = "set_home_object", .size = 1, .n_pop = 2, .n_push = 2, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.define_array_el)] = .{ .name = "define_array_el", .size = 1, .n_pop = 3, .n_push = 2, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.append)] = .{ .name = "append", .size = 1, .n_pop = 3, .n_push = 2, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.copy_data_properties)] = .{ .name = "copy_data_properties", .size = 2, .n_pop = 3, .n_push = 3, .format = .u8, .category = .complex };
    info[@intFromEnum(Opcode.define_method)] = .{ .name = "define_method", .size = 6, .n_pop = 2, .n_push = 1, .format = .atom_u8, .category = .complex };
    info[@intFromEnum(Opcode.define_method_computed)] = .{ .name = "define_method_computed", .size = 2, .n_pop = 3, .n_push = 1, .format = .u8, .category = .complex };
    info[@intFromEnum(Opcode.define_class)] = .{ .name = "define_class", .size = 6, .n_pop = 2, .n_push = 2, .format = .atom_u8, .category = .complex };
    info[@intFromEnum(Opcode.define_class_computed)] = .{ .name = "define_class_computed", .size = 6, .n_pop = 3, .n_push = 3, .format = .atom_u8, .category = .complex };

    // Local/arg access
    info[@intFromEnum(Opcode.get_loc)] = .{ .name = "get_loc", .size = 3, .n_pop = 0, .n_push = 1, .format = .loc, .category = .variable };
    info[@intFromEnum(Opcode.put_loc)] = .{ .name = "put_loc", .size = 3, .n_pop = 1, .n_push = 0, .format = .loc, .category = .variable };
    info[@intFromEnum(Opcode.set_loc)] = .{ .name = "set_loc", .size = 3, .n_pop = 1, .n_push = 1, .format = .loc, .category = .variable };
    info[@intFromEnum(Opcode.get_arg)] = .{ .name = "get_arg", .size = 3, .n_pop = 0, .n_push = 1, .format = .arg, .category = .variable };
    // put_arg modifies argv in-place - Zig codegen uses mutable argv pointer so this is safe
    info[@intFromEnum(Opcode.put_arg)] = .{ .name = "put_arg", .size = 3, .n_pop = 1, .n_push = 0, .format = .arg, .category = .variable };
    info[@intFromEnum(Opcode.set_arg)] = .{ .name = "set_arg", .size = 3, .n_pop = 1, .n_push = 1, .format = .arg, .category = .variable };
    // Generic var_ref opcodes access closure variables - can freeze with native closure support
    info[@intFromEnum(Opcode.get_var_ref)] = .{ .name = "get_var_ref", .size = 3, .n_pop = 0, .n_push = 1, .format = .var_ref, .category = .closure };
    info[@intFromEnum(Opcode.put_var_ref)] = .{ .name = "put_var_ref", .size = 3, .n_pop = 1, .n_push = 0, .format = .var_ref, .category = .closure };
    info[@intFromEnum(Opcode.set_var_ref)] = .{ .name = "set_var_ref", .size = 3, .n_pop = 1, .n_push = 1, .format = .var_ref, .category = .closure };
    info[@intFromEnum(Opcode.set_loc_uninitialized)] = .{ .name = "set_loc_uninitialized", .size = 3, .n_pop = 0, .n_push = 0, .format = .loc, .category = .variable };
    info[@intFromEnum(Opcode.get_loc_check)] = .{ .name = "get_loc_check", .size = 3, .n_pop = 0, .n_push = 1, .format = .loc, .category = .variable };
    info[@intFromEnum(Opcode.put_loc_check)] = .{ .name = "put_loc_check", .size = 3, .n_pop = 1, .n_push = 0, .format = .loc, .category = .variable };
    info[@intFromEnum(Opcode.put_loc_check_init)] = .{ .name = "put_loc_check_init", .size = 3, .n_pop = 1, .n_push = 0, .format = .loc, .category = .variable };
    // var_ref_check opcodes access closure variables with TDZ check - can freeze with native closure support
    info[@intFromEnum(Opcode.get_var_ref_check)] = .{ .name = "get_var_ref_check", .size = 3, .n_pop = 0, .n_push = 1, .format = .var_ref, .category = .closure };
    info[@intFromEnum(Opcode.put_var_ref_check)] = .{ .name = "put_var_ref_check", .size = 3, .n_pop = 1, .n_push = 0, .format = .var_ref, .category = .closure };
    info[@intFromEnum(Opcode.put_var_ref_check_init)] = .{ .name = "put_var_ref_check_init", .size = 3, .n_pop = 1, .n_push = 0, .format = .var_ref, .category = .closure };
    info[@intFromEnum(Opcode.close_loc)] = .{ .name = "close_loc", .size = 3, .n_pop = 0, .n_push = 0, .format = .loc, .category = .variable };

    // Control flow
    info[@intFromEnum(Opcode.if_false)] = .{ .name = "if_false", .size = 5, .n_pop = 1, .n_push = 0, .format = .label, .category = .control_flow };
    info[@intFromEnum(Opcode.if_true)] = .{ .name = "if_true", .size = 5, .n_pop = 1, .n_push = 0, .format = .label, .category = .control_flow };
    info[@intFromEnum(Opcode.goto)] = .{ .name = "goto", .size = 5, .n_pop = 0, .n_push = 0, .format = .label, .category = .control_flow };
    // catch/gosub/ret/nip_catch require runtime exception handling which frozen interpreter doesn't support
    info[@intFromEnum(Opcode.@"catch")] = .{ .name = "catch", .size = 5, .n_pop = 0, .n_push = 1, .format = .label, .category = .never_freeze };
    info[@intFromEnum(Opcode.gosub)] = .{ .name = "gosub", .size = 5, .n_pop = 0, .n_push = 0, .format = .label, .category = .never_freeze };
    info[@intFromEnum(Opcode.ret)] = .{ .name = "ret", .size = 1, .n_pop = 1, .n_push = 0, .format = .none, .category = .never_freeze };
    info[@intFromEnum(Opcode.nip_catch)] = .{ .name = "nip_catch", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .never_freeze };

    // Type conversion
    info[@intFromEnum(Opcode.to_object)] = .{ .name = "to_object", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.to_propkey)] = .{ .name = "to_propkey", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .complex };
    // to_propkey2 used in computed property destructuring - converts key to property key while preserving object on stack
    info[@intFromEnum(Opcode.to_propkey2)] = .{ .name = "to_propkey2", .size = 1, .n_pop = 2, .n_push = 2, .format = .none, .category = .complex };

    // With statement (never freeze)
    info[@intFromEnum(Opcode.with_get_var)] = .{ .name = "with_get_var", .size = 10, .n_pop = 1, .n_push = 0, .format = .atom_label_u8, .category = .never_freeze };
    info[@intFromEnum(Opcode.with_put_var)] = .{ .name = "with_put_var", .size = 10, .n_pop = 2, .n_push = 1, .format = .atom_label_u8, .category = .never_freeze };
    info[@intFromEnum(Opcode.with_delete_var)] = .{ .name = "with_delete_var", .size = 10, .n_pop = 1, .n_push = 0, .format = .atom_label_u8, .category = .never_freeze };
    info[@intFromEnum(Opcode.with_make_ref)] = .{ .name = "with_make_ref", .size = 10, .n_pop = 1, .n_push = 0, .format = .atom_label_u8, .category = .never_freeze };
    info[@intFromEnum(Opcode.with_get_ref)] = .{ .name = "with_get_ref", .size = 10, .n_pop = 1, .n_push = 0, .format = .atom_label_u8, .category = .never_freeze };
    info[@intFromEnum(Opcode.with_get_ref_undef)] = .{ .name = "with_get_ref_undef", .size = 10, .n_pop = 1, .n_push = 0, .format = .atom_label_u8, .category = .never_freeze };

    // Make ref - creates runtime variable references, cannot be frozen
    // Used by compound assignment operators (||=, ??=, &&=) and closures
    info[@intFromEnum(Opcode.make_loc_ref)] = .{ .name = "make_loc_ref", .size = 7, .n_pop = 0, .n_push = 2, .format = .atom_u16, .category = .never_freeze };
    info[@intFromEnum(Opcode.make_arg_ref)] = .{ .name = "make_arg_ref", .size = 7, .n_pop = 0, .n_push = 2, .format = .atom_u16, .category = .never_freeze };
    info[@intFromEnum(Opcode.make_var_ref_ref)] = .{ .name = "make_var_ref_ref", .size = 7, .n_pop = 0, .n_push = 2, .format = .atom_u16, .category = .never_freeze };
    info[@intFromEnum(Opcode.make_var_ref)] = .{ .name = "make_var_ref", .size = 5, .n_pop = 0, .n_push = 2, .format = .atom, .category = .never_freeze };

    // Iterators (for-in/for-of via wrapper functions)
    info[@intFromEnum(Opcode.for_in_start)] = .{ .name = "for_in_start", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .iterator };
    info[@intFromEnum(Opcode.for_of_start)] = .{ .name = "for_of_start", .size = 1, .n_pop = 1, .n_push = 3, .format = .none, .category = .iterator };
    info[@intFromEnum(Opcode.for_await_of_start)] = .{ .name = "for_await_of_start", .size = 1, .n_pop = 1, .n_push = 3, .format = .none, .category = .never_freeze };
    info[@intFromEnum(Opcode.for_in_next)] = .{ .name = "for_in_next", .size = 1, .n_pop = 1, .n_push = 3, .format = .none, .category = .iterator };
    info[@intFromEnum(Opcode.for_of_next)] = .{ .name = "for_of_next", .size = 2, .n_pop = 3, .n_push = 5, .format = .u8, .category = .iterator };
    info[@intFromEnum(Opcode.iterator_check_object)] = .{ .name = "iterator_check_object", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.iterator_get_value_done)] = .{ .name = "iterator_get_value_done", .size = 1, .n_pop = 1, .n_push = 2, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.iterator_close)] = .{ .name = "iterator_close", .size = 1, .n_pop = 3, .n_push = 0, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.iterator_next)] = .{ .name = "iterator_next", .size = 1, .n_pop = 4, .n_push = 4, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.iterator_call)] = .{ .name = "iterator_call", .size = 2, .n_pop = 4, .n_push = 5, .format = .u8, .category = .complex };
    info[@intFromEnum(Opcode.initial_yield)] = .{ .name = "initial_yield", .size = 1, .n_pop = 0, .n_push = 0, .format = .none, .category = .never_freeze };
    info[@intFromEnum(Opcode.yield)] = .{ .name = "yield", .size = 1, .n_pop = 1, .n_push = 2, .format = .none, .category = .never_freeze };
    info[@intFromEnum(Opcode.yield_star)] = .{ .name = "yield_star", .size = 1, .n_pop = 1, .n_push = 2, .format = .none, .category = .never_freeze };
    info[@intFromEnum(Opcode.async_yield_star)] = .{ .name = "async_yield_star", .size = 1, .n_pop = 1, .n_push = 2, .format = .none, .category = .never_freeze };
    info[@intFromEnum(Opcode.await)] = .{ .name = "await", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .never_freeze };

    // Arithmetic
    info[@intFromEnum(Opcode.neg)] = .{ .name = "neg", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.plus)] = .{ .name = "plus", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.dec)] = .{ .name = "dec", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.inc)] = .{ .name = "inc", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.post_dec)] = .{ .name = "post_dec", .size = 1, .n_pop = 1, .n_push = 2, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.post_inc)] = .{ .name = "post_inc", .size = 1, .n_pop = 1, .n_push = 2, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.dec_loc)] = .{ .name = "dec_loc", .size = 2, .n_pop = 0, .n_push = 0, .format = .loc8, .category = .arithmetic };
    info[@intFromEnum(Opcode.inc_loc)] = .{ .name = "inc_loc", .size = 2, .n_pop = 0, .n_push = 0, .format = .loc8, .category = .arithmetic };
    info[@intFromEnum(Opcode.add_loc)] = .{ .name = "add_loc", .size = 2, .n_pop = 1, .n_push = 0, .format = .loc8, .category = .arithmetic };
    info[@intFromEnum(Opcode.not)] = .{ .name = "not", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.lnot)] = .{ .name = "lnot", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.typeof)] = .{ .name = "typeof", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.delete)] = .{ .name = "delete", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.delete_var)] = .{ .name = "delete_var", .size = 5, .n_pop = 0, .n_push = 1, .format = .atom, .category = .complex };
    info[@intFromEnum(Opcode.mul)] = .{ .name = "mul", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.div)] = .{ .name = "div", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.mod)] = .{ .name = "mod", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.add)] = .{ .name = "add", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.sub)] = .{ .name = "sub", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.shl)] = .{ .name = "shl", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.sar)] = .{ .name = "sar", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.shr)] = .{ .name = "shr", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.@"and")] = .{ .name = "and", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.xor)] = .{ .name = "xor", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.@"or")] = .{ .name = "or", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };
    info[@intFromEnum(Opcode.pow)] = .{ .name = "pow", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .arithmetic };

    // Comparison
    info[@intFromEnum(Opcode.lt)] = .{ .name = "lt", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .comparison };
    info[@intFromEnum(Opcode.lte)] = .{ .name = "lte", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .comparison };
    info[@intFromEnum(Opcode.gt)] = .{ .name = "gt", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .comparison };
    info[@intFromEnum(Opcode.gte)] = .{ .name = "gte", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .comparison };
    info[@intFromEnum(Opcode.instanceof)] = .{ .name = "instanceof", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.in)] = .{ .name = "in", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.eq)] = .{ .name = "eq", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .comparison };
    info[@intFromEnum(Opcode.neq)] = .{ .name = "neq", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .comparison };
    info[@intFromEnum(Opcode.strict_eq)] = .{ .name = "strict_eq", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .comparison };
    info[@intFromEnum(Opcode.strict_neq)] = .{ .name = "strict_neq", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .comparison };
    info[@intFromEnum(Opcode.is_undefined_or_null)] = .{ .name = "is_undefined_or_null", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.private_in)] = .{ .name = "private_in", .size = 1, .n_pop = 2, .n_push = 1, .format = .none, .category = .complex };
    info[@intFromEnum(Opcode.push_bigint_i32)] = .{ .name = "push_bigint_i32", .size = 5, .n_pop = 0, .n_push = 1, .format = .i32, .category = .simple };
    info[@intFromEnum(Opcode.nop)] = .{ .name = "nop", .size = 1, .n_pop = 0, .n_push = 0, .format = .none, .category = .simple };

    // Short forms
    info[@intFromEnum(Opcode.push_minus1)] = .{ .name = "push_minus1", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_int, .category = .simple };
    info[@intFromEnum(Opcode.push_0)] = .{ .name = "push_0", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_int, .category = .simple };
    info[@intFromEnum(Opcode.push_1)] = .{ .name = "push_1", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_int, .category = .simple };
    info[@intFromEnum(Opcode.push_2)] = .{ .name = "push_2", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_int, .category = .simple };
    info[@intFromEnum(Opcode.push_3)] = .{ .name = "push_3", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_int, .category = .simple };
    info[@intFromEnum(Opcode.push_4)] = .{ .name = "push_4", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_int, .category = .simple };
    info[@intFromEnum(Opcode.push_5)] = .{ .name = "push_5", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_int, .category = .simple };
    info[@intFromEnum(Opcode.push_6)] = .{ .name = "push_6", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_int, .category = .simple };
    info[@intFromEnum(Opcode.push_7)] = .{ .name = "push_7", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_int, .category = .simple };
    info[@intFromEnum(Opcode.push_i8)] = .{ .name = "push_i8", .size = 2, .n_pop = 0, .n_push = 1, .format = .i8, .category = .simple };
    info[@intFromEnum(Opcode.push_i16)] = .{ .name = "push_i16", .size = 3, .n_pop = 0, .n_push = 1, .format = .i16, .category = .simple };
    // push_const8 - push constant from pool (now supported via static cpool pointer)
    info[@intFromEnum(Opcode.push_const8)] = .{ .name = "push_const8", .size = 2, .n_pop = 0, .n_push = 1, .format = .const8, .category = .simple };
    info[@intFromEnum(Opcode.fclosure8)] = .{ .name = "fclosure8", .size = 2, .n_pop = 0, .n_push = 1, .format = .const8, .category = .never_freeze };
    info[@intFromEnum(Opcode.push_empty_string)] = .{ .name = "push_empty_string", .size = 1, .n_pop = 0, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.get_loc8)] = .{ .name = "get_loc8", .size = 2, .n_pop = 0, .n_push = 1, .format = .loc8, .category = .variable };
    info[@intFromEnum(Opcode.put_loc8)] = .{ .name = "put_loc8", .size = 2, .n_pop = 1, .n_push = 0, .format = .loc8, .category = .variable };
    info[@intFromEnum(Opcode.set_loc8)] = .{ .name = "set_loc8", .size = 2, .n_pop = 1, .n_push = 1, .format = .loc8, .category = .variable };
    info[@intFromEnum(Opcode.get_loc0_loc1)] = .{ .name = "get_loc0_loc1", .size = 1, .n_pop = 0, .n_push = 2, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.get_loc0)] = .{ .name = "get_loc0", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.get_loc1)] = .{ .name = "get_loc1", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.get_loc2)] = .{ .name = "get_loc2", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.get_loc3)] = .{ .name = "get_loc3", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.put_loc0)] = .{ .name = "put_loc0", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.put_loc1)] = .{ .name = "put_loc1", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.put_loc2)] = .{ .name = "put_loc2", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.put_loc3)] = .{ .name = "put_loc3", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.set_loc0)] = .{ .name = "set_loc0", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.set_loc1)] = .{ .name = "set_loc1", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.set_loc2)] = .{ .name = "set_loc2", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.set_loc3)] = .{ .name = "set_loc3", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_loc, .category = .variable };
    info[@intFromEnum(Opcode.get_arg0)] = .{ .name = "get_arg0", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_arg, .category = .variable };
    info[@intFromEnum(Opcode.get_arg1)] = .{ .name = "get_arg1", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_arg, .category = .variable };
    info[@intFromEnum(Opcode.get_arg2)] = .{ .name = "get_arg2", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_arg, .category = .variable };
    info[@intFromEnum(Opcode.get_arg3)] = .{ .name = "get_arg3", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_arg, .category = .variable };
    // put_argN modifies argv in-place - Zig codegen uses mutable argv pointer so this is safe
    info[@intFromEnum(Opcode.put_arg0)] = .{ .name = "put_arg0", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_arg, .category = .variable };
    info[@intFromEnum(Opcode.put_arg1)] = .{ .name = "put_arg1", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_arg, .category = .variable };
    info[@intFromEnum(Opcode.put_arg2)] = .{ .name = "put_arg2", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_arg, .category = .variable };
    info[@intFromEnum(Opcode.put_arg3)] = .{ .name = "put_arg3", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_arg, .category = .variable };
    info[@intFromEnum(Opcode.set_arg0)] = .{ .name = "set_arg0", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_arg, .category = .variable };
    info[@intFromEnum(Opcode.set_arg1)] = .{ .name = "set_arg1", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_arg, .category = .variable };
    info[@intFromEnum(Opcode.set_arg2)] = .{ .name = "set_arg2", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_arg, .category = .variable };
    info[@intFromEnum(Opcode.set_arg3)] = .{ .name = "set_arg3", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_arg, .category = .variable };
    // get_var_ref0/1/2/3 access closure variables - can freeze with native closure support
    info[@intFromEnum(Opcode.get_var_ref0)] = .{ .name = "get_var_ref0", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_var_ref, .category = .closure };
    info[@intFromEnum(Opcode.get_var_ref1)] = .{ .name = "get_var_ref1", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_var_ref, .category = .closure };
    info[@intFromEnum(Opcode.get_var_ref2)] = .{ .name = "get_var_ref2", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_var_ref, .category = .closure };
    info[@intFromEnum(Opcode.get_var_ref3)] = .{ .name = "get_var_ref3", .size = 1, .n_pop = 0, .n_push = 1, .format = .none_var_ref, .category = .closure };
    // put/set_var_ref0/1/2/3 modify closure variables - can freeze with native closure support
    info[@intFromEnum(Opcode.put_var_ref0)] = .{ .name = "put_var_ref0", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_var_ref, .category = .closure };
    info[@intFromEnum(Opcode.put_var_ref1)] = .{ .name = "put_var_ref1", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_var_ref, .category = .closure };
    info[@intFromEnum(Opcode.put_var_ref2)] = .{ .name = "put_var_ref2", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_var_ref, .category = .closure };
    info[@intFromEnum(Opcode.put_var_ref3)] = .{ .name = "put_var_ref3", .size = 1, .n_pop = 1, .n_push = 0, .format = .none_var_ref, .category = .closure };
    info[@intFromEnum(Opcode.set_var_ref0)] = .{ .name = "set_var_ref0", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_var_ref, .category = .closure };
    info[@intFromEnum(Opcode.set_var_ref1)] = .{ .name = "set_var_ref1", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_var_ref, .category = .closure };
    info[@intFromEnum(Opcode.set_var_ref2)] = .{ .name = "set_var_ref2", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_var_ref, .category = .closure };
    info[@intFromEnum(Opcode.set_var_ref3)] = .{ .name = "set_var_ref3", .size = 1, .n_pop = 1, .n_push = 1, .format = .none_var_ref, .category = .closure };
    info[@intFromEnum(Opcode.get_length)] = .{ .name = "get_length", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .property };
    info[@intFromEnum(Opcode.if_false8)] = .{ .name = "if_false8", .size = 2, .n_pop = 1, .n_push = 0, .format = .label8, .category = .control_flow };
    info[@intFromEnum(Opcode.if_true8)] = .{ .name = "if_true8", .size = 2, .n_pop = 1, .n_push = 0, .format = .label8, .category = .control_flow };
    info[@intFromEnum(Opcode.goto8)] = .{ .name = "goto8", .size = 2, .n_pop = 0, .n_push = 0, .format = .label8, .category = .control_flow };
    info[@intFromEnum(Opcode.goto16)] = .{ .name = "goto16", .size = 3, .n_pop = 0, .n_push = 0, .format = .label16, .category = .control_flow };
    info[@intFromEnum(Opcode.call0)] = .{ .name = "call0", .size = 1, .n_pop = 1, .n_push = 1, .format = .npopx, .category = .control_flow };
    info[@intFromEnum(Opcode.call1)] = .{ .name = "call1", .size = 1, .n_pop = 1, .n_push = 1, .format = .npopx, .category = .control_flow };
    info[@intFromEnum(Opcode.call2)] = .{ .name = "call2", .size = 1, .n_pop = 1, .n_push = 1, .format = .npopx, .category = .control_flow };
    info[@intFromEnum(Opcode.call3)] = .{ .name = "call3", .size = 1, .n_pop = 1, .n_push = 1, .format = .npopx, .category = .control_flow };
    info[@intFromEnum(Opcode.is_undefined)] = .{ .name = "is_undefined", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.is_null)] = .{ .name = "is_null", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.typeof_is_undefined)] = .{ .name = "typeof_is_undefined", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .simple };
    info[@intFromEnum(Opcode.typeof_is_function)] = .{ .name = "typeof_is_function", .size = 1, .n_pop = 1, .n_push = 1, .format = .none, .category = .simple };

    break :blk info;
};

/// Get opcode info
pub fn getInfo(op: Opcode) OpcodeInfo {
    return opcode_info[@intFromEnum(op)];
}

/// Get opcode info by raw byte
pub fn getInfoByByte(byte: u8) OpcodeInfo {
    return opcode_info[byte];
}

/// Check if opcode is a jump instruction
pub fn isJump(op: Opcode) bool {
    return switch (op) {
        .goto, .goto8, .goto16, .if_false, .if_true, .if_false8, .if_true8, .@"catch", .gosub => true,
        else => false,
    };
}

/// Check if opcode terminates a basic block
pub fn isTerminator(op: Opcode) bool {
    return switch (op) {
        .@"return", .return_undef, .return_async, .throw, .ret, .tail_call, .tail_call_method => true,
        else => false,
    };
}

/// Check if opcode can be frozen (not in never_freeze category)
pub fn canFreeze(op: Opcode) bool {
    return getInfo(op).category != .never_freeze;
}

/// Get implicit integer value for push_N opcodes
pub fn getImplicitInt(op: Opcode) ?i32 {
    return switch (op) {
        .push_minus1 => -1,
        .push_0 => 0,
        .push_1 => 1,
        .push_2 => 2,
        .push_3 => 3,
        .push_4 => 4,
        .push_5 => 5,
        .push_6 => 6,
        .push_7 => 7,
        else => null,
    };
}

/// Get implicit local index for get_locN/put_locN/set_locN opcodes
pub fn getImplicitLocal(op: Opcode) ?u16 {
    return switch (op) {
        .get_loc0, .put_loc0, .set_loc0 => 0,
        .get_loc1, .put_loc1, .set_loc1 => 1,
        .get_loc2, .put_loc2, .set_loc2 => 2,
        .get_loc3, .put_loc3, .set_loc3 => 3,
        else => null,
    };
}

/// Get implicit arg index for get_argN/put_argN/set_argN opcodes
pub fn getImplicitArg(op: Opcode) ?u16 {
    return switch (op) {
        .get_arg0, .put_arg0, .set_arg0 => 0,
        .get_arg1, .put_arg1, .set_arg1 => 1,
        .get_arg2, .put_arg2, .set_arg2 => 2,
        .get_arg3, .put_arg3, .set_arg3 => 3,
        else => null,
    };
}

/// Get implicit call argc for callN opcodes
pub fn getImplicitCallArgc(op: Opcode) ?u16 {
    return switch (op) {
        .call0 => 0,
        .call1 => 1,
        .call2 => 2,
        .call3 => 3,
        else => null,
    };
}

test "opcode info" {
    const add_info = getInfo(.add);
    try std.testing.expectEqualStrings("add", add_info.name);
    try std.testing.expectEqual(@as(u8, 1), add_info.size);
    try std.testing.expectEqual(@as(i8, 2), add_info.n_pop);
    try std.testing.expectEqual(@as(i8, 1), add_info.n_push);
    try std.testing.expectEqual(Category.arithmetic, add_info.category);

    const push_i32_info = getInfo(.push_i32);
    try std.testing.expectEqual(@as(u8, 5), push_i32_info.size);

    const if_false_info = getInfo(.if_false);
    try std.testing.expectEqual(@as(u8, 5), if_false_info.size);
    try std.testing.expect(isJump(.if_false));
    try std.testing.expect(!isJump(.add));

    try std.testing.expect(isTerminator(.@"return"));
    try std.testing.expect(!isTerminator(.add));

    try std.testing.expect(canFreeze(.add));
    try std.testing.expect(!canFreeze(.eval));
    try std.testing.expect(!canFreeze(.yield));
}
