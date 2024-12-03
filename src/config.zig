pub const Config = struct {
    max_locals: u16 = 256,
    initial_code_size: u32 = 512,
    initial_data_size: u32 = 512,

    pub fn LocalType(comptime self: Config) type {
        const ml = self.max_locals;
        if (ml == 0) @compileError("max_locals must be greater than 0");
        if (ml <= 256) return u8;
        if (ml <= 65536) return u16;
        @compileError("max_locals must be less than 65536");
    }
};
