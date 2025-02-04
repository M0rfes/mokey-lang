use crate::ast;
use crate::object::Object;
use crate::token::Token;

const TRUE_OBJ: Object = Object::Boolean(true);
const FALSE_OBJ: Object = Object::Boolean(false);
const NULL_OBJ: Object = Object::Null;

pub fn eval_program<'a>(node: ast::Program) -> Object<'a> {
    let mut result = NULL_OBJ;
    for statement in node.statements {
        result = eval_statement(statement);
    }
    result
}

fn eval_statement<'a>(statement: Box<dyn ast::Statement>) -> Object<'a> {
    match statement
        .as_any()
        .downcast_ref::<ast::ExpressionStatement>()
    {
        Some(expr_stmt) => eval_expression(&expr_stmt.0),
        _ => NULL_OBJ,
    }
}

fn eval_expression<'a>(expression: &Box<dyn ast::Expression>) -> Object<'a> {
    if let Some(int) = expression.as_any().downcast_ref::<ast::Int>() {
        return Object::Integer(int.0);
    } else if let Some(float) = expression.as_any().downcast_ref::<ast::Float>() {
        return Object::Float(float.0);
    } else if let Some(boolean) = expression.as_any().downcast_ref::<ast::Bool>() {
        if boolean.0 {
            return TRUE_OBJ;
        } else {
            return FALSE_OBJ;
        }
    } else if let Some(string) = expression.as_any().downcast_ref::<ast::StringLiteral>() {
        return Object::StringLiteral(string.0.clone());
    } else if let Some(prefix) = expression.as_any().downcast_ref::<ast::Prefix>() {
        let right = eval_expression(&prefix.right);
        return eval_prefix_expression(&prefix.operator, right);
    } else {
        return Object::Null;
    }
}

fn eval_prefix_expression<'a>(operator: &Token, right: Object<'a>) -> Object<'a> {
    match operator {
        Token::Not => eval_bang_operator_expression(right),
        Token::Sub => eval_minus_prefix_operator_expression(right),
        Token::BitwiseNot => eval_bitwise_operator_expression(right),
        Token::Increment => eval_increment_operator_expression(right),
        Token::Decrement => eval_decrement_operator_expression(right),
        _ => NULL_OBJ,
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(b) => {
            if b {
                return FALSE_OBJ;
            } else {
                return TRUE_OBJ;
            }
        }
        Object::Integer(n) => {
            if n == 0 {
                return TRUE_OBJ;
            } else {
                return FALSE_OBJ;
            }
        }
        Object::Float(n) => {
            if n == 0.0 {
                return TRUE_OBJ;
            } else {
                return FALSE_OBJ;
            }
        }
        Object::StringLiteral(s) => {
            if s.is_empty() {
                return TRUE_OBJ;
            } else {
                return FALSE_OBJ;
            }
        }
        Object::Null => return TRUE_OBJ,
        _ => return FALSE_OBJ,
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(n) => Object::Integer(-n),
        Object::Float(n) => Object::Float(-n),
        _ => NULL_OBJ,
    }
}

fn eval_bitwise_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(n) => Object::Integer(!n),
        Object::Float(n) => Object::Integer(!(n as i128)),
        Object::Boolean(b) => Object::Boolean(!b),
        _ => NULL_OBJ,
        
    }
}

fn eval_increment_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(n) => Object::Integer(n + 1),
        Object::Float(n) => Object::Float(n + 1.0),
        _ => NULL_OBJ,
    }
}

fn eval_decrement_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(n) => Object::Integer(n - 1),
        Object::Float(n) => Object::Float(n - 1.0),
        _ => NULL_OBJ,
    }
}