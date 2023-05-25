use std::{collections::HashMap, fmt::Display};

use crate::ast::{BlockStatement, Identifier};

#[derive(Debug, Clone)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
    String(String),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Func(FuncObject),
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
            _ => true,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(int) => write!(f, "{}", int),
            Self::Boolean(bool) => write!(f, "{}", bool),
            Self::String(string) => write!(f, "\"{}\"", string),
            Self::Null => write!(f, "null"),
            Self::ReturnValue(val) => write!(f, "{}", *val),
            Self::Error(err) => write!(f, "[Error]: {}", err),
            Self::Func(func) => {
                write!(f, "fn (")?;

                let mut params = func.params.iter().peekable();

                while let Some(param) = params.next() {
                    if params.peek().is_some() {
                        write!(f, "{}, ", param)?;
                    } else {
                        write!(f, "{}", param)?;
                    }
                }

                write!(f, ") {}", func.body)?;

                Ok(())
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncObject {
    pub params: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl<'a> Environment {
    pub fn new() -> Environment {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, key: String) -> Option<&Object> {
        self.store.get(&key)
    }

    pub fn set(&mut self, key: String, value: Object) {
        self.store.insert(key, value);
    }
}
