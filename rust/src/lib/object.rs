use core::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast;

#[derive(Clone)]
pub enum Object {
    StringLiteral(String),
    Integer(i128),
    Float(f64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Null, // Null is a singleton, so we use a Box<()>
    Error(Vec<String>),
    Function(Rc<Function>),
}

// Object trait is no longer needed
// pub trait Object: Any + fmt::Display + Default {
//     fn object_type(&self) -> ObjectType;
//     fn as_any(&self) -> &dyn Any;
// }

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::StringLiteral(s) => write!(f, "\"{}\"", s),
            Object::Integer(n) => write!(f, "{}", n),
            Object::Float(n) => write!(f, "{}", n),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(obj) => write!(f, "{}", obj),
            Object::Error(msgs) => {
                let mut s = String::new();
                for msg in msgs {
                    s.push_str(msg);
                }
                write!(f, "{}", s)
            }
            Object::Function(func) => write!(f, "{}", func),
        }
    }
}

pub struct Function {
    pub function_literal: ast::FunctionLiteral,
    pub env: Rc<RefCell<Environment>>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.function_literal)
    }
}

pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.store.get(name) {
            Some(obj) => Some(obj.clone()),
            None => match &self.outer {
                Some(outer) => outer.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }
}
