pub const Version = enum {
    GL_VERSION_1_0,
    GL_VERSION_1_1,
    GL_VERSION_1_2,
    GL_VERSION_1_3,
    GL_VERSION_1_4,
    GL_VERSION_1_5,
    GL_VERSION_2_0,
    GL_VERSION_2_1,
    GL_VERSION_3_0,
    GL_VERSION_3_1,
    GL_VERSION_3_2,
    GL_VERSION_3_3,
    GL_VERSION_4_0,
    GL_VERSION_4_1,
    GL_VERSION_4_2,
    GL_VERSION_4_3,
    GL_VERSION_4_4,
    GL_VERSION_4_5,
    GL_VERSION_4_6,
    GL_VERSION_ES_CM_1_0,
    GL_ES_VERSION_2_0,
    GL_ES_VERSION_3_0,
    GL_ES_VERSION_3_1,
    GL_ES_VERSION_3_2,
    GL_SC_VERSION_2_0,
};

pub const Profile = enum {
    core,
    common,
    compatibility,
};

pub const Api = enum {
    gl,
    gles1,
    gles2,
    glsc2,
};
