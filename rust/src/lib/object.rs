use super::token;

use super::ast;

pub trait Object {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}
#[derive(Debug, PartialEq)]
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

impl Object for Integer {
    fn object_type(&self) -> ObjectType {
        ObjectType::Integer(self.value)
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

pub struct Boolean {
    pub value: bool,
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

impl Object for StringObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::StringObj(self.value.clone())
    }
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

pub struct Null;

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::Null
    }
    fn inspect(&self) -> String {
        format!("null")
    }
}
