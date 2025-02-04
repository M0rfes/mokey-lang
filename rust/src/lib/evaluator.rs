use crate::ast;
use crate::object::Object;

const TRUE_OBJ: bool = true;
const FALSE_OBJ: bool = false;
const NULL_OBJ: Object = Object::Null;

pub fn eval_program(node: ast::Program) -> Object {
    let mut result = NULL_OBJ;
    for statement in node.statements {
        result = eval_statement(statement);
    }
    result
}

fn eval_statement(statement: Box<dyn ast::Statement>) -> Object {
    match statement
        .as_any()
        .downcast_ref::<ast::ExpressionStatement>()
    {
        Some(expr_stmt) => eval_expression(&expr_stmt.0),
        _ => NULL_OBJ,
    }
}

fn eval_expression(expression: &Box<dyn ast::Expression>) -> Object {
    if let Some(int) = expression.as_any().downcast_ref::<ast::Int>() {
        return Object::Integer(Box::new(int.0));
    } else if let Some(float) = expression.as_any().downcast_ref::<ast::Float>() {
        return Object::Float(Box::new(float.0));
    } else if let Some(boolean) = expression.as_any().downcast_ref::<ast::Bool>() {
        return Object::Boolean(Box::new(boolean.0));
    } else {
        return Object::Null;
    }
}
