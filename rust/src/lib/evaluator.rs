use crate::object::Object;
use crate::token::Token;
use crate::{ast, object};

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
    } else if let Some(infix) = expression.as_any().downcast_ref::<ast::Infix>() {
        let left = eval_expression(&infix.left);
        let right = eval_expression(&infix.right);
        return eval_infix_expression(left, &infix.operator, right);
    } else if let Some(postfix) = expression.as_any().downcast_ref::<ast::Postfix>() {
        let right = eval_expression(&postfix.left);
        return eval_postfix_expression(&postfix.operator, right);
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
        Object::StringLiteral(s) => {
            let new_s = s
                .chars()
                .map(|c| {
                    if c == 'z' {
                        // Handle 'z' specially to wrap around to 'a'
                        'a'
                    } else if c == 'Z' {
                        'A'
                    } else  {
                        // Only shift alphabetic characters
                        (c as u8 + 1) as char // Increment ASCII value and convert back to char
                    }
                })
                .collect();
            Object::StringLiteral(new_s)
        }
        _ => NULL_OBJ,
    }
}

fn eval_decrement_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(n) => Object::Integer(n - 1),
        Object::Float(n) => Object::Float(n - 1.0),
        Object::StringLiteral(s) => {
            let new_s = s
                .chars()
                .map(|c| {
                    if c == 'a' {
                        // Handle 'z' specially to wrap around to 'a'
                        'z'
                    } else if c == 'A' {
                        'Z'
                    } else  {
                        // Only shift alphabetic characters
                        (c as u8 - 1) as char // Increment ASCII value and convert back to char
                    }
                })
                .collect();
            Object::StringLiteral(new_s)
        }
        _ => NULL_OBJ,
    }
}

fn eval_infix_expression<'a>(left: Object<'a>, operator: &Token, right: Object<'a>) -> Object<'a> {
    match operator {
        Token::Add => eval_plus_infix_expression(left, right),
        Token::Sub => eval_minus_infix_expression(left, right),
        Token::Mul => eval_multiply_infix_expression(left, right),
        Token::Div => eval_divide_infix_expression(left, right),
        Token::Mod => eval_modulo_infix_expression(left, right),
        Token::BitwiseAnd => eval_bitwise_and_infix_expression(left, right),
        Token::BitwiseOr => eval_bitwise_or_infix_expression(left, right),
        Token::BitwiseXor => eval_bitwise_xor_infix_expression(left, right),
        Token::ShiftLeft => eval_bitwise_left_shift_infix_expression(left, right),
        Token::ShiftRight => eval_bitwise_right_shift_infix_expression(left, right),
        Token::Equal => eval_equal_infix_expression(left, right),
        Token::NotEqual => eval_not_equal_infix_expression(left, right),
        Token::Less => eval_less_infix_expression(left, right),
        Token::LessEq => eval_less_eq_infix_expression(left, right),
        Token::Greater => eval_greater_infix_expression(left, right),
        Token::GreaterEq => eval_greater_eq_infix_expression(left, right),
        Token::LogicalAnd => {
            if is_truthy(left) && is_truthy(right) {
                return TRUE_OBJ;
            } else {
                return FALSE_OBJ;
            }
        }
        Token::LogicalOr => {
            if is_truthy(left) || is_truthy(right) {
                return TRUE_OBJ;
            } else {
                return FALSE_OBJ;
            }
        }
        Token::LogicalXor => {
            if is_truthy(left) ^ is_truthy(right) {
                return TRUE_OBJ;
            } else {
                return FALSE_OBJ;
            }
        }
        _ => NULL_OBJ,
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Boolean(b) => b,
        Object::Integer(n) => n != 0,
        Object::Float(n) => n != 0.0,
        Object::StringLiteral(s) => !s.is_empty(),
        Object::Null => false,
        _ => true,
    }
}

fn eval_plus_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l + r),
        (Object::Float(l), Object::Float(r)) => Object::Float(l + r),
        (Object::Integer(l), Object::Float(r)) => Object::Float(l as f64 + r),
        (Object::Float(l), Object::Integer(r)) => Object::Float(l + r as f64),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::StringLiteral(l + &r),
        _ => NULL_OBJ,
    }
}

fn eval_minus_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l - r),
        (Object::Float(l), Object::Float(r)) => Object::Float(l - r),
        (Object::Integer(l), Object::Float(r)) => Object::Float(l as f64 - r),
        (Object::Float(l), Object::Integer(r)) => Object::Float(l - r as f64),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => {
            Object::StringLiteral(l.replace(&r, ""))
        }
        _ => NULL_OBJ,
    }
}

fn eval_multiply_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l * r),
        (Object::Float(l), Object::Float(r)) => Object::Float(l * r),
        (Object::Integer(l), Object::Float(r)) => Object::Float(l as f64 * r),
        (Object::Float(l), Object::Integer(r)) => Object::Float(l * r as f64),
        (Object::StringLiteral(l), Object::Integer(r)) => {
            Object::StringLiteral(l.repeat(r as usize))
        }
        (Object::StringLiteral(l), Object::Float(r)) => {
            Object::StringLiteral(l.repeat(r as usize as usize))
        }
        (Object::Integer(l), Object::StringLiteral(r)) => {
            Object::StringLiteral(r.repeat(l as usize))
        }
        (Object::Float(l), Object::StringLiteral(r)) => {
            Object::StringLiteral(r.repeat(l as usize as usize))
        }
        (Object::StringLiteral(l), Object::StringLiteral(r)) => {
            let zip: String = l
                .chars()
                .zip(r.chars())
                .map(|(a, b)| a.to_string() + &b.to_string())
                .collect();
            Object::StringLiteral(zip)
        }
        _ => NULL_OBJ,
    }
}

fn eval_divide_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l / r),
        (Object::Float(l), Object::Float(r)) => Object::Float(l / r),
        (Object::Integer(l), Object::Float(r)) => Object::Float(l as f64 / r),
        (Object::Float(l), Object::Integer(r)) => Object::Float(l / r as f64),
        (Object::StringLiteral(l), Object::Integer(r)) => {
            let cut_off = if l.len() > r as usize {
                l.len() - r as usize
            } else {
                0
            };
            Object::StringLiteral(l[..cut_off as usize].to_string())
        }
        (Object::StringLiteral(l), Object::Float(r)) => {
            let cut_off = if l.len() > r as usize as usize {
                l.len() - r as usize as usize
            } else {
                0
            };
            Object::StringLiteral(l[..cut_off].to_string())
        }
        (Object::StringLiteral(l), Object::StringLiteral(r)) => {
            Object::StringLiteral(l.chars().filter(|c| !r.contains(*c)).collect())
        }
        (Object::Integer(l), Object::StringLiteral(r)) => {
            let start = if l as usize > r.len() {
                r.len()
            } else {
                l as usize
            };
            Object::StringLiteral(r[start..].to_string())
        }

        (Object::Float(l), Object::StringLiteral(r)) => {
            let start = if l as usize > r.len() {
                r.len()
            } else {
                l as usize
            };
            Object::StringLiteral(r[start..].to_string())
        }
        _ => NULL_OBJ,
    }
}

fn eval_modulo_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l % r),
        (Object::Float(l), Object::Float(r)) => Object::Float(l % r),
        (Object::Integer(l), Object::Float(r)) => Object::Float(l as f64 % r),
        (Object::Float(l), Object::Integer(r)) => Object::Float(l % r as f64),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Integer(
            l.chars()
                .filter(|c| !r.contains(*c))
                .collect::<String>()
                .len() as i128,
        ),
        (Object::StringLiteral(l), Object::Integer(r)) => {
            Object::StringLiteral(l.chars().rev().cycle().take(r as usize).collect())
        }
        (Object::StringLiteral(l), Object::Float(r)) => {
            Object::StringLiteral(l.chars().rev().cycle().take(r as usize as usize).collect())
        }
        (Object::Integer(l), Object::StringLiteral(r)) => {
            Object::StringLiteral(r.chars().cycle().take(l as usize).collect())
        }
        (Object::Float(l), Object::StringLiteral(r)) => {
            Object::StringLiteral(r.chars().cycle().take(l as usize as usize).collect())
        }
        _ => NULL_OBJ,
    }
}

fn eval_bitwise_and_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l & r),
        (Object::Float(l), Object::Float(r)) => Object::Integer(l as i128 & r as i128),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l & r),
        (Object::Integer(l), Object::Boolean(r)) => Object::Integer(l & r as i128),
        (Object::Boolean(l), Object::Integer(r)) => Object::Integer(l as i128 & r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => {
            Object::StringLiteral(l.chars().filter(|c| r.contains(*c)).collect())
        }
        _ => NULL_OBJ,
    }
}

fn eval_bitwise_or_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l | r),
        (Object::Float(l), Object::Float(r)) => Object::Integer(l as i128 | r as i128),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l | r),
        (Object::Integer(l), Object::Boolean(r)) => Object::Integer(l | r as i128),
        (Object::Boolean(l), Object::Integer(r)) => Object::Integer(l as i128 | r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => {
            let set = l
                .chars()
                .chain(r.chars())
                .collect::<std::collections::HashSet<char>>();
            Object::StringLiteral(set.into_iter().collect())
        }
        _ => NULL_OBJ,
    }
}

fn eval_bitwise_xor_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l ^ r),
        (Object::Float(l), Object::Float(r)) => Object::Integer(l as i128 ^ r as i128),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l ^ r),
        (Object::Integer(l), Object::Boolean(r)) => Object::Integer(l ^ r as i128),
        (Object::Boolean(l), Object::Integer(r)) => Object::Integer(l as i128 ^ r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => {
            let intersection = l
                .chars()
                .filter(|c| r.contains(*c))
                .collect::<std::collections::HashSet<char>>();
            Object::StringLiteral(
                l.chars()
                    .chain(r.chars())
                    .filter(|c| !intersection.contains(c))
                    .collect(),
            )
        }
        _ => NULL_OBJ,
    }
}

fn eval_bitwise_left_shift_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l << r),
        (Object::Float(l), Object::Float(r)) => Object::Integer((l as i128) << (r as i128)),
        (Object::Boolean(l), Object::Boolean(r)) => {
            Object::Boolean(((l as i128) << (r as i128)) as i128 != 0)
        }
        (Object::Integer(l), Object::Boolean(r)) => Object::Integer(l << r as i128),
        (Object::Boolean(l), Object::Integer(r)) => Object::Integer((l as i128) << r),
        (Object::StringLiteral(l), Object::Integer(r)) => {
            Object::StringLiteral(l.to_string() + &" ".repeat(r as usize))
        }
        (Object::StringLiteral(l), Object::Float(r)) => {
            Object::StringLiteral(l.to_string() + &" ".repeat(r as usize as usize))
        }
        _ => NULL_OBJ,
    }
}

fn eval_bitwise_right_shift_infix_expression<'a>(
    left: Object<'a>,
    right: Object<'a>,
) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l >> r),
        (Object::Float(l), Object::Float(r)) => Object::Integer((l as i128) >> (r as i128)),
        (Object::Boolean(l), Object::Boolean(r)) => {
            Object::Boolean(((l as i128) >> (r as i128)) as i128 != 0)
        }
        (Object::Integer(l), Object::Boolean(r)) => Object::Integer(l >> r as i128),
        (Object::Boolean(l), Object::Integer(r)) => Object::Integer((l as i128) >> r),
        (Object::StringLiteral(l), Object::Integer(r)) => {
            Object::StringLiteral(" ".repeat(r as usize) + &l)
        }
        (Object::StringLiteral(l), Object::Float(r)) => {
            Object::StringLiteral(" ".repeat(r as usize as usize) + &l)
        }
        _ => NULL_OBJ,
    }
}

fn eval_equal_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l == r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l == r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l == r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l == r),
        _ => Object::Boolean(false),
    }
}

fn eval_not_equal_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l != r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l != r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l != r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l != r),
        _ => Object::Boolean(true),
    }
}

fn eval_less_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l < r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l < r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l < r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l < r),
        _ => Object::Boolean(false),
    }
}

fn eval_less_eq_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l <= r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l <= r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l <= r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l <= r),
        _ => Object::Boolean(false),
    }
}

fn eval_greater_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l > r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l > r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l > r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l > r),
        _ => Object::Boolean(false),
    }
}

fn eval_greater_eq_infix_expression<'a>(left: Object<'a>, right: Object<'a>) -> Object<'a> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l >= r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l >= r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l >= r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l >= r),
        _ => Object::Boolean(false),
    }
}

fn eval_postfix_expression<'a>(operator: &Token, right: Object<'a>) -> Object<'a> {
    match operator {
        Token::Increment => eval_increment_operator_expression(right),
        Token::Decrement => eval_decrement_operator_expression(right),
        _ => NULL_OBJ,
    }
}
