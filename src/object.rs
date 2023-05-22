use std::fmt::Display;

#[derive(Debug)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(int) => write!(f, "{}", int),
            Self::Boolean(bool) => write!(f, "{}", bool),
            Self::Null => write!(f, "null"),
        }
    }
}
