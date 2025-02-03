use core::fmt;

pub enum ObjectType {
    StringLiteral(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Null,
}

pub trait Object: fmt::Display {
    fn object_type(&self) -> ObjectType;
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectType::StringLiteral(s) => write!(f, "{}", s),
            ObjectType::Float(n) => write!(f, "{}", n),
            ObjectType::Int(n) => write!(f, "{}", n),
            ObjectType::Bool(b) => write!(f, "{}", b),
            ObjectType::Null => write!(f, "null"),
        }
    }
}

pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn object_type(&self) -> ObjectType {
        ObjectType::Int(self.value)
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
    
}



pub struct Boolean {
    pub value: bool,
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
    
}

impl Object for Boolean {
    fn object_type(&self) -> ObjectType {
        ObjectType::Bool(self.value)
    }
}

pub struct Null;

impl Object for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::Null
    }
}

impl fmt::Display for Null {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "null")
    }
    
}

pub struct StringLiteral {
    value: std::string::String,
}

impl Object for StringLiteral {
    fn object_type(&self) -> ObjectType {
        ObjectType::StringLiteral(self.value.clone())
    }
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
    
}

pub struct Float {
    value: f64,
}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
    
}

impl Object for Float {
    fn object_type(&self) -> ObjectType {
        ObjectType::Float(self.value)
    }
}
