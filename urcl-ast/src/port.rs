#[derive(Debug, Clone, strum::EnumString, strum::FromRepr)]
#[strum(ascii_case_insensitive)]
pub enum Port {
    // general
    CpuBus    = 0,
    Text      = 1,
    Numb      = 2,
    Supported = 5,
    Special   = 6,
    Profile   = 7,
    // graphics
    X         = 8,
    Y         = 9,
    #[strum(serialize = "color", serialize = "colour")]
    Color     = 10,
    Buffer    = 11,
    GSpecial  = 15,
    // text
    Ascii8    = 16,
    Char5     = 17,
    Char6     = 18,
    Ascii7    = 19,
    Utf8      = 20,
    TSpecial  = 23,
    // numbers
    Int       = 24,
    Uint      = 25,
    Bin       = 26,
    Hex       = 27,
    Float     = 28,
    Fixed     = 29,
    NSpecial  = 31,
    // storage
    Addr      = 32,
    Bus       = 33,
    Page      = 34,
    SSpecial  = 39,
    // miscellaneous
    Rng       = 40,
    Note      = 41,
    Instr     = 42,
    NLeg      = 43,
    Wait      = 44,
    NAddr     = 45,
    Data      = 46,
    MSpecial  = 47,
    // user defined
    Ud1       = 48,
    Ud2       = 49,
    Ud3       = 50,
    Ud4       = 51,
    Ud5       = 52,
    Ud6       = 53,
    Ud7       = 54,
    Ud8       = 55,
    Ud9       = 56,
    Ud10      = 57,
    Ud11      = 58,
    Ud12      = 59,
    Ud13      = 60,
    Ud14      = 61,
    Ud15      = 62,
    Ud16      = 63,
}
