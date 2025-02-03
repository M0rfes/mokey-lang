use crate::ast;
use crate::object;
use crate::token::Token;

const TRUE_OBJ: object::Boolean = object::Boolean { value: true };
const FALSE_OBJ: object::Boolean = object::Boolean { value: false };
const NULL_OBJ: object::Null = object::Null {};

pub fn eval_program(node: ast::Program) -> Option<Box<dyn object::Object>> {
    let mut result = None;
    for statement in node.statements {
        result = eval_statement(statement);
    }
    result
}

fn eval_statement(statement: Box<dyn ast::Statement>) -> Option<Box<dyn object::Object>> {
    match statement
        .as_any()
        .downcast_ref::<ast::ExpressionStatement>()
    {
        Some(expression_statement) => eval_expression(&expression_statement.0),
        None => Some(Box::new(NULL_OBJ)),
    }
}

fn eval_expression(expression: &Box<dyn ast::Expression>) -> Option<Box<dyn object::Object>> {
    if let Some(integer_literal) = expression.as_any().downcast_ref::<ast::Int>() {
        Some(Box::new(object::Integer {
            value: integer_literal.0,
        }))
    } else if let Some(boolean_literal) = expression.as_any().downcast_ref::<ast::Bool>() {
        if boolean_literal.0 {
            Some(Box::new(TRUE_OBJ))
        } else {
            Some(Box::new(FALSE_OBJ))
        }
    } else if let Some(string_literal) = expression.as_any().downcast_ref::<ast::StringLiteral>() {
        Some(Box::new(object::StringLiteral {
            value: string_literal.0.clone(),
        }))
    } else if let Some(float_literal) = expression.as_any().downcast_ref::<ast::Float>() {
        Some(Box::new(object::Float {
            value: float_literal.0,
        }))
    } else if let Some(prefix_expression) = expression.as_any().downcast_ref::<ast::Prefix>() {
        let right = eval_expression(&prefix_expression.right);
        match right {
            Some(right) => eval_prefix_expression(&prefix_expression.operator, right),
            None => None,
        }
    } else if let Some(infix_expression) = expression.as_any().downcast_ref::<ast::Infix>() {
        let left = eval_expression(&infix_expression.left);
        let right = eval_expression(&infix_expression.right);

        match (left, right) {
            (Some(left), Some(right)) => {
                eval_infix_expression(&infix_expression.operator, left, right)
            }
            _ => None,
        }
    } else {
        None
    }
}

fn eval_prefix_expression(
    operator: &Token,
    right: Box<dyn object::Object>,
) -> Option<Box<dyn object::Object>> {
    match operator {
        Token::Not => eval_bang_operator_expression(right),
        Token::Sub => eval_minus_prefix_operator_expression(right),
        Token::BitwiseNot => eval_bitwise_not_operator_expression(right),
        _ => None,
    }
}

fn eval_bang_operator_expression(
    right: Box<dyn object::Object>,
) -> Option<Box<dyn object::Object>> {
    match right.object_type() {
        object::ObjectType::Bool(value) => {
            if value {
                Some(Box::new(FALSE_OBJ))
            } else {
                Some(Box::new(TRUE_OBJ))
            }
        }
        object::ObjectType::Null => Some(Box::new(TRUE_OBJ)),
        _ => None,
    }
}

fn eval_minus_prefix_operator_expression(
    right: Box<dyn object::Object>,
) -> Option<Box<dyn object::Object>> {
    match right.object_type() {
        object::ObjectType::Int(value) => Some(Box::new(object::Integer { value: -value })),
        object::ObjectType::Float(value) => Some(Box::new(object::Float { value: -value })),
        _ => None,
    }
}

fn eval_bitwise_not_operator_expression(
    right: Box<dyn object::Object>,
) -> Option<Box<dyn object::Object>> {
    match right.object_type() {
        object::ObjectType::Int(value) => Some(Box::new(object::Integer { value: !value })),
        object::ObjectType::Bool(value) => Some(Box::new(object::Integer {
            value: !value as i64,
        })),
        object::ObjectType::Float(value) => {
            let int_value = value as i64;
            Some(Box::new(object::Integer { value: !int_value }))
        },
        _ => None,
    }
}

fn eval_infix_expression(
    operator: &Token,
    left: Box<dyn object::Object>,
    right: Box<dyn object::Object>,
) -> Option<Box<dyn object::Object>> {
    match (left.object_type(), right.object_type()) {
        (object::ObjectType::Int(left_value), object::ObjectType::Int(right_value)) => {
            eval_integer_infix_expression(operator, left_value, right_value)
        }
        (object::ObjectType::Float(left_value), object::ObjectType::Float(right_value)) => {
            eval_float_infix_expression(operator, left_value, right_value)
        }
        (object::ObjectType::Int(left_value), object::ObjectType::Float(right_value)) => {
            eval_float_infix_expression(operator, left_value as f64, right_value)
        }
        (object::ObjectType::Float(left_value), object::ObjectType::Int(right_value)) => {
            eval_float_infix_expression(operator, left_value, right_value as f64)
        }
        (object::ObjectType::Bool(left_value), object::ObjectType::Bool(right_value)) => {
            eval_boolean_infix_expression(operator, left_value, right_value)
        }
        _ => None,
    }
}

fn eval_integer_infix_expression(
    operator: &Token,
    left: i64,
    right: i64,
) -> Option<Box<dyn object::Object>> {
    match operator {
        Token::Add => Some(Box::new(object::Integer {
            value: left + right,
        })),
        Token::Sub => Some(Box::new(object::Integer {
            value: left - right,
        })),
        Token::Mul => Some(Box::new(object::Integer {
            value: left * right,
        })),
        Token::Div => Some(Box::new(object::Integer {
            value: left / right,
        })),
        Token::Mod => Some(Box::new(object::Integer {
            value: left % right,
        })),
        Token::Power => Some(Box::new(object::Integer {
            value: left.pow(right as u32),
        })),
        Token::Less => {
            if left < right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::LessEq => {
            if left <= right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::Greater => {
            if left > right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::GreaterEq => {
            if left >= right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::Equal => {
            if left == right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::NotEqual => {
            if left != right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::LogicalAnd => Some(Box::new(object::Integer {
            value: left & right,
        })),
        Token::LogicalOr => Some(Box::new(object::Integer {
            value: left | right,
        })),
        Token::LogicalXor => Some(Box::new(object::Integer {
            value: left ^ right,
        })),
        Token::BitwiseAnd => Some(Box::new(object::Integer {
            value: left & right,
        })),
        Token::BitwiseOr => Some(Box::new(object::Integer {
            value: left | right,
        })),
        Token::BitwiseXor => Some(Box::new(object::Integer {
            value: left ^ right,
        })),
        Token::ShiftLeft => Some(Box::new(object::Integer {
            value: left << right,
        })),
        Token::ShiftRight => Some(Box::new(object::Integer {
            value: left >> right,
        })),
        _ => None,
    }
}

fn eval_float_infix_expression(
    operator: &Token,
    left: f64,
    right: f64,
) -> Option<Box<dyn object::Object>> {
    match operator {
        Token::Add => Some(Box::new(object::Float {
            value: left + right,
        })),
        Token::Sub => Some(Box::new(object::Float {
            value: left - right,
        })),
        Token::Mul => Some(Box::new(object::Float {
            value: left * right,
        })),
        Token::Div => Some(Box::new(object::Float {
            value: left / right,
        })),
        Token::Mod => Some(Box::new(object::Float {
            value: left % right,
        })),
        Token::Power => Some(Box::new(object::Float {
            value: left.powf(right),
        })),
        Token::Less => {
            if left < right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::LessEq => {
            if left <= right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::Greater => {
            if left > right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::GreaterEq => {
            if left >= right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::Equal => {
            if left == right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::NotEqual => {
            if left != right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::LogicalAnd => {
            let l = left as i64;
            let r = right as i64;

            Some(Box::new(object::Integer { value: l & r }))
        }

        Token::LogicalOr => {
            let l = left as i64;
            let r = right as i64;

            Some(Box::new(object::Integer { value: l | r }))
        }

        Token::LogicalXor => {
            let l = left as i64;
            let r = right as i64;

            Some(Box::new(object::Integer { value: l ^ r }))
        }

        Token::BitwiseAnd => {
            let l = left as i64;
            let r = right as i64;

            Some(Box::new(object::Integer { value: l & r }))
        }

        Token::BitwiseOr => {
            let l = left as i64;
            let r = right as i64;

            Some(Box::new(object::Integer { value: l | r }))
        }

        Token::BitwiseXor => {
            let l = left as i64;
            let r = right as i64;

            Some(Box::new(object::Integer { value: l ^ r }))
        }

        Token::ShiftLeft => {
            let l = left as i64;
            let r = right as u32;

            Some(Box::new(object::Integer { value: l << r }))
        }

        Token::ShiftRight => {
            let l = left as i64;
            let r = right as u32;

            Some(Box::new(object::Integer { value: l >> r }))
        }
        _ => None,
    }
}

fn eval_boolean_infix_expression(
    operator: &Token,
    left: bool,
    right: bool,
) -> Option<Box<dyn object::Object>> {
    match operator {
        Token::Equal => {
            if left == right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::NotEqual => {
            if left != right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::LogicalOr => {
            if left || right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::LogicalAnd => {
            if left && right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::LogicalXor => {
            if left ^ right {
                Some(Box::new(TRUE_OBJ))
            } else {
                Some(Box::new(FALSE_OBJ))
            }
        }
        Token::BitwiseAnd => Some(Box::new(object::Integer {
            value: left as i64 & right as i64,
        })),
        Token::BitwiseOr => Some(Box::new(object::Integer {
            value: left as i64 | right as i64,
        })),
        Token::BitwiseXor => Some(Box::new(object::Integer {
            value: left as i64 ^ right as i64,
        })),
        Token::ShiftLeft => {
            let r = right as u32;
            let l = left as i64;
            Some(Box::new(object::Integer { value: l << r }))
        }
        Token::ShiftRight => Some(Box::new(object::Integer {
            value: left as i64 >> right as u32,
        })),
        _ => None,
    }
}
