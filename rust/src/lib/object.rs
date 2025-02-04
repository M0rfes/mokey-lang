use core::fmt;

pub enum Object {
    StringLiteral(Box<String>),
    Integer(Box<i128>),
    Float(Box<f64>),
    Boolean(Box<bool>),
    ReturnValue(Box<Object>),
    Null// Null is a singleton, so we use a Box<()>
}

// Object trait is no longer needed
// pub trait Object: Any + fmt::Display + Default {
//     fn object_type(&self) -> ObjectType;
//     fn as_any(&self) -> &dyn Any;
// }


impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::StringLiteral(s) => write!(f, "{}", s),
            Object::Integer(n) => write!(f, "{}", n),
            Object::Float(n) => write!(f, "{}", n),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(obj) => write!(f, "{}", obj),
        }
    }
}
