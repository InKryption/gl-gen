pub const Version = enum {
    VERSION_1_0,
    VERSION_1_1,
    VERSION_1_2,
    VERSION_1_3,
    VERSION_1_4,
    VERSION_1_5,
    VERSION_2_0,
    VERSION_2_1,
    VERSION_3_0,
    VERSION_3_1,
    VERSION_3_2,
    VERSION_3_3,
    VERSION_4_0,
    VERSION_4_1,
    VERSION_4_2,
    VERSION_4_3,
    VERSION_4_4,
    VERSION_4_5,
    VERSION_4_6,
    VERSION_ES_CM_1_0,
    ES_VERSION_2_0,
    ES_VERSION_3_0,
    ES_VERSION_3_1,
    ES_VERSION_3_2,
    SC_VERSION_2_0,

    pub fn stringWithGlPrefix(version: Version) []const u8 {
        return switch (version) {
            inline else => |tag| "GL_" ++ @tagName(tag),
        };
    }
};

pub const Profile = enum {
    core,
    common,
    compatibility,
};
