#[derive(Debug, Clone, PartialEq, Eq, Hash, strum::Display, strum::EnumIs)]
#[strum(serialize_all = "lowercase")]
pub enum Register {
    #[strum(serialize = "r{0}")]
    General(u16),
    Pc,
    Sp,
}
