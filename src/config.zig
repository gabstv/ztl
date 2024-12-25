pub const DebugMode = enum {
    none,
    minimal,
    full,
};

const Defaults = struct {
    pub const debug: DebugMode = .none;
    pub const max_locals: u16 = 256;
    pub const max_call_frames: u8 = 255;
    pub const initial_code_size: u32 = 512;
    pub const initial_data_size: u32 = 512;
    pub const deduplicate_string_literals: bool = true;
    pub const allow_leaks: bool = true;
};

pub fn extract(comptime A: type, comptime field_name: []const u8) @TypeOf(@field(Defaults, field_name)) {
    const App = switch (@typeInfo(A)) {
        .@"struct" => A,
        .pointer => |ptr| ptr.child,
        .void => void,
        else => @compileError("Template App must be a struct, got: " ++ @tagName(@typeInfo(A))),
    };

    if (App != void and @hasDecl(App, "ZtlConfig") and @hasDecl(App.ZtlConfig, field_name)) {
        return @field(App.ZtlConfig, field_name);
    }

    return @field(Defaults, field_name);
}

pub fn shouldDebug(comptime App: type, level: DebugMode) bool {
    const configured_debug_level = extract(App, "debug");
    return @intFromEnum(configured_debug_level) >= @intFromEnum(level);
}

pub fn LocalType(comptime App: type) type {
    const ml = extract(App, "max_locals");
    if (ml == 0) @compileError("max_locals must be greater than 0");
    if (ml <= 256) return u8;
    if (ml <= 65536) return u16;
    @compileError("max_locals must be less than 65536");
}
