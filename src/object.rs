use std::{collections::{HashMap, hash_map::DefaultHasher}, fmt::Display, hash::{Hash, Hasher}};

use crate::{
    ast::{BlockStatement, Identifier},
    built_in_function::BuiltInFunction,
};

#[derive(Debug, Clone)]
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

#[derive(Debug, Clone)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
    String(String),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Func(FuncObject),
    BuiltInFunc(BuiltInFunction),
    Array(Vec<Object>),
    Hash(HashMap<u64, HashPair>),
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

    // TODO: error handling
    pub fn get_hash_key(&self) -> u64 {
        match self {
            Self::Boolean(bool) => if *bool { 1 } else { 0 },
            Self::Integer(int) => u64::try_from(*int).unwrap(),
            Self::String(string) => calculate_hash(string),
            _ => todo!(),
        }
    }
}

fn calculate_hash<T: Hash>(val: &T) -> u64 {
    let mut s = DefaultHasher::new();
    val.hash(&mut s);
    s.finish()
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
            Self::BuiltInFunc(_) => write!(f, "builtin function"),
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
            Self::Array(elems) => {
                write!(f, "[")?;

                let mut elems = elems.iter().peekable();

                while let Some(elem) = elems.next() {
                    if elems.peek().is_some() {
                        write!(f, "{}, ", elem)?;
                    } else {
                        write!(f, "{}", elem)?;
                    }
                }

                write!(f, "]")
            },
            Self::Hash(hash_pairs) => {
                write!(f, "{{")?;

                let mut entries = hash_pairs.iter().peekable();
                while let Some((_, entry)) = entries.next() {
                    if entries.peek().is_some() {
                        write!(f, "{}: {}, ", entry.key, entry.value)?;
                    } else {
                        write!(f, "{}: {}", entry.key, entry.value)?;
                    }
                }

                write!(f, "}}")
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

#[cfg(test)]
mod tests {
    use super::Object;

    #[test]
    fn string_hash_keys_are_equal() {
        let equal_1 = Object::String("Hello world".to_string());
        let equal_2 = Object::String("Hello world".to_string());
        
        let diff_1 = Object::String("foo bar".to_string());

        assert_eq!(equal_1.get_hash_key(), equal_2.get_hash_key());
        assert!(equal_1.get_hash_key() != diff_1.get_hash_key());
    }

    #[test]
    fn boolean_hash_keys_are_equal() {
        let equal_1 = Object::Boolean(true);
        let equal_2 = Object::Boolean(true);
        
        let diff_1 = Object::Boolean(false);

        assert_eq!(equal_1.get_hash_key(), equal_2.get_hash_key());
        assert!(equal_1.get_hash_key() != diff_1.get_hash_key());
    }

    #[test]
    fn int_hash_keys_are_equal() {
        let equal_1 = Object::Integer(42);
        let equal_2 = Object::Integer(42);
        
        let diff_1 = Object::Integer(69);

        assert_eq!(equal_1.get_hash_key(), equal_2.get_hash_key());
        assert!(equal_1.get_hash_key() != diff_1.get_hash_key());
    }
}
