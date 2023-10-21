use super::token;

use super::ast;

trait Object: Into<ObjectType> + TryFrom<Box<dyn ast::Node>> {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}
pub enum ObjectType {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    StringObj(String),
    Null,
}

pub struct Float {
    pub value: f64,
}

impl Into<ObjectType> for Float {
    fn into(self) -> ObjectType {
        ObjectType::Float(self.value)
    }
}

impl TryFrom<Box<dyn ast::Node>> for Float {
    type Error = &'static str;
    fn try_from(node: Box<dyn ast::Node>) -> Result<Self, Self::Error> {
        match node.token() {
            &token::Token::INT(value) => Ok(Float {
                value: value as f64,
            }),
            &token::Token::FLOAT(value) => Ok(Float { value }),
            &token::Token::TRUE => Ok(Float { value: 1.0 }),
            &token::Token::FALSE => Ok(Float { value: 0.0 }),
            &token::Token::STRING(ref s) => {
                let float = s.parse::<f64>();
                match float {
                    Ok(value) => Ok(Float { value }),
                    Err(_) => Err("Float::from called with non-float token"),
                }
            }
            _ => Err("Float::from called with non-float token"),
        }
    }
}

impl Object for Float {
    fn object_type(&self) -> ObjectType {
        ObjectType::Float(self.value)
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

pub struct Integer {
    pub value: i64,
}

impl Into<ObjectType> for Integer {
    fn into(self) -> ObjectType {
        ObjectType::Integer(self.value)
    }
}

impl TryFrom<Box<dyn ast::Node>> for Integer {
    type Error = &'static str;
    fn try_from(node: Box<dyn ast::Node>) -> Result<Self, Self::Error> {
        match node.token() {
            &token::Token::INT(value) => Ok(Integer { value }),
            &token::Token::FLOAT(value) => Ok(Integer {
                value: value as i64,
            }),
            &token::Token::TRUE => Ok(Integer { value: 1 }),
            &token::Token::FALSE => Ok(Integer { value: 0 }),
            &token::Token::STRING(ref s) => {
                let int = s.parse::<i64>();
                match int {
                    Ok(value) => Ok(Integer { value }),
                    Err(_) => Err("Integer::from called with non-integer token"),
                }
            }
            _ => Err("Integer::from called with non-integer token"),
        }
    }
}

impl Object for Integer {
    fn object_type(&self) -> ObjectType {
        ObjectType::Integer(self.value)
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

impl TryFrom<Box<dyn ast::Node>> for Boolean {
    type Error = &'static str;
    fn try_from(value: Box<dyn ast::Node>) -> Result<Self, Self::Error> {
        match value.token() {
            &token::Token::TRUE => Ok(Boolean { value: true }),
            &token::Token::FALSE => Ok(Boolean { value: false }),
            &token::Token::INT(0) => Ok(Boolean { value: false }),
            &token::Token::INT(_) => Ok(Boolean { value: true }),
            &token::Token::STRING(ref s) => Ok(Boolean {
                value: !s.is_empty(),
            }),
            _ => Err("Boolean::from called with non-boolean token"),
        }
    }
}

pub struct Boolean {
    pub value: bool,
}

impl Into<ObjectType> for Boolean {
    fn into(self) -> ObjectType {
        ObjectType::Boolean(self.value)
    }
}

impl Object for Boolean {
    fn object_type(&self) -> ObjectType {
        ObjectType::Boolean(self.value)
    }
    fn inspect(&self) -> String {
        format!("{}", self.value.to_string())
    }
}

pub struct StringObj {
    pub value: String,
}

impl Into<ObjectType> for StringObj {
    fn into(self) -> ObjectType {
        ObjectType::StringObj(self.value)
    }
}

impl TryFrom<Box<dyn ast::Node>> for StringObj {
    type Error = &'static str;
    fn try_from(value: Box<dyn ast::Node>) -> Result<Self, Self::Error> {
        match value.token() {
            &token::Token::STRING(ref s) => Ok(StringObj {
                value: s.to_string(),
            }),
            &token::Token::INT(value) => Ok(StringObj {
                value: value.to_string(),
            }),
            &token::Token::FLOAT(value) => Ok(StringObj {
                value: value.to_string(),
            }),
            &token::Token::TRUE => Ok(StringObj {
                value: "true".to_string(),
            }),
            &token::Token::FALSE => Ok(StringObj {
                value: "false".to_string(),
            }),
            _ => Err("String::from called with non-string token"),
        }
    }
}

pub struct Null;

impl Into<ObjectType> for Null {
    fn into(self) -> ObjectType {
        ObjectType::Null
    }
}

impl From<Box<dyn ast::Node>> for Null {
    fn from(_node: Box<dyn ast::Node>) -> Self {
        Null
    }
}

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::Null
    }
    fn inspect(&self) -> String {
        format!("null")
    }
}
