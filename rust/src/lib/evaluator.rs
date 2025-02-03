use crate::ast;
use crate::object;

pub fn eval_program(node: ast::Program) -> Option<Box<dyn object::Object>> {
    let mut result = None;
    for statement in node.statements {
        result = eval_statement(statement);
    }
    result
}



fn eval_statement(statement: Box<dyn ast::Statement>) -> Option<Box<dyn object::Object>> {
    match statement.as_any().downcast_ref::<ast::ExpressionStatement>() {
        Some(expression_statement) => eval_expression(&expression_statement.0),
        None => None,
    }
}

fn eval_expression(expression: &Box<dyn ast::Expression>) -> Option<Box<dyn object::Object>> {
    if let Some(integer_literal) = expression.as_any().downcast_ref::<ast::Int>() {
        Some(Box::new(object::Integer { value: integer_literal.0 }))
    } else if let Some(boolean_literal) = expression.as_any().downcast_ref::<ast::Bool>() {
        Some(Box::new(object::Boolean { value: boolean_literal.0 }))
    } else {
        None
    }
}