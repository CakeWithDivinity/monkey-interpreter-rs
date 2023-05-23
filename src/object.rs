use std::fmt::Display;

#[derive(Debug)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
}

impl Object {
    pub fn is_error(&self) -> bool {
        match self {
            Self::Error(_) => true,
            _ => false,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Null | Self::Boolean(false) => false,
            Self::Boolean(true) => true,
            Self::Integer(_) | Self::ReturnValue(_) | Self::Error(_) => true,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(int) => write!(f, "{}", int),
            Self::Boolean(bool) => write!(f, "{}", bool),
            Self::Null => write!(f, "null"),
            Self::ReturnValue(val) => write!(f, "{}", *val),
            Self::Error(err) => write!(f, "[Error]: {}", err),
        }
    }
}
