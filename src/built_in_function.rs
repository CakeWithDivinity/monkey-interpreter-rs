use crate::object::Object;

#[derive(Debug, Clone)]
pub enum BuiltInFunction {
    LEN,
    FIRST,
    LAST,
    TAIL,
    PUSH,
    PUTS,
}

impl BuiltInFunction {
    pub fn from_identifier(identifier: &String) -> Option<BuiltInFunction> {
        match identifier.as_str() {
            "len" => Some(BuiltInFunction::LEN),
            "first" => Some(BuiltInFunction::FIRST),
            "last" => Some(BuiltInFunction::LAST),
            "tail" => Some(BuiltInFunction::TAIL),
            "push" => Some(Self::PUSH),
            "puts" => Some(Self::PUTS),
            _ => None,
        }
    }

    pub fn execute(&self, args: Vec<Object>) -> Object {
        match self {
            Self::LEN => {
                let Some(arg) = args.first() else {
                    return Object::Error("len(..) requires 1 argument".to_string());
                };

                match arg {
                    Object::String(string) => {
                        let Ok(length) = string.len().try_into() else {
                            return Object::Error("String is too long to be a valid integer".to_string());
                        };
                        Object::Integer(length)
                    }
                    Object::Array(arr) => match arr.len().try_into() {
                        Ok(len) => Object::Integer(len),
                        Err(_) => Object::Error(
                            "Array length is too long to be a valid integer".to_string(),
                        ),
                    },
                    arg => Object::Error(format!("cannot call built-in function `len` on {}", arg)),
                }
            }
            Self::FIRST => {
                let Some(arg) = args.first() else {
                    return Object::Error("first(..) requires 1 argument".to_string());
                };

                match arg {
                    Object::Array(arr) => arr.first().cloned().unwrap_or(Object::Null),
                    arg => {
                        Object::Error(format!("cannot call built-in function `first` on {}", arg))
                    }
                }
            }
            Self::LAST => {
                let Some(arg) = args.first() else {
                    return Object::Error("last(..) requires 1 argument".to_string());
                };

                match arg {
                    Object::Array(arr) => arr.last().cloned().unwrap_or(Object::Null),
                    arg => {
                        Object::Error(format!("cannot call built-in function `last` on {}", arg))
                    }
                }
            }
            Self::TAIL => {
                let Some(arg) = args.first() else {
                    return Object::Error("tail(..) requires 1 argument".to_string());
                };

                match arg {
                    Object::Array(arr) if arr.is_empty() => Object::Null,
                    Object::Array(arr) if !arr.is_empty() => Object::Array(arr[1..].to_vec()),
                    arg => {
                        Object::Error(format!("cannot call built-in function `tail` on {}", arg))
                    }
                }
            }
            Self::PUSH => {
                let Some([arr, elem]) = args.get(0..2) else {
                    return Object::Error("push(..) requires 2 arguments".to_string());
                };

                match arr {
                    Object::Array(arr) => {
                        let mut arr = arr.clone();
                        arr.push(elem.clone());

                        Object::Array(arr)
                    }
                    arg => Object::Error(format!(
                        "expected first argument of `push` to be Array. Got {:?}",
                        arg
                    )),
                }
            },
            Self::PUTS => {
                for arg in args {
                    println!("{}", arg);
                }
                
                Object::Null
            },
        }
    }
}
