use crate::object::Object;

#[derive(Debug, Clone)]
pub enum BuiltInFunction {
    LEN,
}

impl BuiltInFunction {
    pub fn from_identifier(identifier: &String) -> Option<BuiltInFunction> {
        match identifier.as_str() {
            "len" => Some(BuiltInFunction::LEN),
            _ => None,
        }
    }

    pub fn execute(&self, args: Vec<Object>) -> Option<Object> {
        match self {
            Self::LEN => {
                let Some(arg) = args.first() else {
                    return None;
                };

                match arg {
                    Object::String(string) => {
                        let length: isize = string.len().try_into().ok()?;
                        Some(Object::Integer(length))
                    }
                    _ => None,
                }
            }
        }
    }
}
