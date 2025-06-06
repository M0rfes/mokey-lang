use std::cell::RefCell;
use std::rc::Rc;

use crate::object::{Builtins, Object};
use crate::token::Token;
use crate::{ast, object};

const TRUE_OBJ: Object = Object::Boolean(true);
const FALSE_OBJ: Object = Object::Boolean(false);
const NULL_OBJ: Object = Object::Null;

pub fn eval_program(node: ast::Program, env: Rc<RefCell<object::Environment>>) -> Box<Object> {
    let mut result = NULL_OBJ;
    for statement in node.statements {
        result = eval_statement(&statement, env.clone());
        if let Object::ReturnValue(obj) = result {
            return obj;
        }
        if let Object::Error(_) = result {
            return Box::new(result);
        }
    }
    Box::new(result)
}

fn eval_statement(
    statement: &Box<dyn ast::Statement>,
    env: Rc<RefCell<object::Environment>>,
) -> Object {
    if let Some(return_statement) = statement.as_any().downcast_ref::<ast::ReturnStatement>() {
        return Object::ReturnValue(Box::new(eval_expression(&return_statement.0, env)));
    }
    if let Some(block_statement) = statement.as_any().downcast_ref::<ast::BlockStatement>() {
        return eval_block_statement(block_statement, env);
    }
    if let Some(let_statement) = statement.as_any().downcast_ref::<ast::LetStatement>() {
        return eval_let_statement(let_statement, env);
    }
    match statement
        .as_any()
        .downcast_ref::<ast::ExpressionStatement>()
    {
        Some(expr_stmt) => eval_expression(&expr_stmt.0, env),
        _ => NULL_OBJ,
    }
}

fn eval_block_statement(
    block: &ast::BlockStatement,
    env: Rc<RefCell<object::Environment>>,
) -> Object {
    let mut result = NULL_OBJ;
    for statement in &block.statements {
        result = eval_statement(statement, env.clone());
        if let Object::ReturnValue(_) = result {
            return result;
        }
        if let Object::Error(_) = result {
            return result;
        }
    }
    result
}

fn eval_expression(
    expression: &Box<dyn ast::Expression>,
    env: Rc<RefCell<object::Environment>>,
) -> Object {
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
    } else if let Some(if_expression) = expression.as_any().downcast_ref::<ast::IfExpression>() {
        return eval_if_expression(if_expression, env);
    } else if let Some(prefix) = expression.as_any().downcast_ref::<ast::Prefix>() {
        let right = eval_expression(&prefix.right, env);
        return eval_prefix_expression(&prefix.operator, right);
    } else if let Some(infix) = expression.as_any().downcast_ref::<ast::Infix>() {
        let left = eval_expression(&infix.left, env.clone());
        let right = eval_expression(&infix.right, env.clone());
        return eval_infix_expression(left, &infix.operator, right);
    } else if let Some(postfix) = expression.as_any().downcast_ref::<ast::Postfix>() {
        let right = eval_expression(&postfix.left, env);
        return eval_postfix_expression(&postfix.operator, right);
    } else if let Some(ident) = expression.as_any().downcast_ref::<ast::Identifier>() {
        return eval_identifier(ident, env.clone());
    } else if let Some(function_expression) =
        expression.as_any().downcast_ref::<ast::FunctionLiteral>()
    {
        let function_object = object::Function {
            function_literal: function_expression.clone(),
            env: env.clone(),
        };
        return Object::Function(Rc::new(function_object));
    } else if let Some(call_expression) = expression.as_any().downcast_ref::<ast::CallExpression>()
    {
        let function = eval_expression(&call_expression.function, env.clone());
        let args = call_expression
            .arguments
            .iter()
            .map(|arg| eval_expression(arg, env.clone()))
            .collect();
        return apply_function(function, args);
    } else if let Some(array_literal) = expression.as_any().downcast_ref::<ast::ArrayLiteral>() {
        return eval_array_expression(array_literal, env);
    } else if let Some(index_expression) = expression.as_any().downcast_ref::<ast::IndexExpression>() {
        let left = eval_expression(&index_expression.left, env.clone());
        let index = eval_expression(&index_expression.index, env.clone());
        return eval_index_expression(left, index);
    } else {
        return Object::Null;
    }
}

fn apply_function(function: Object, args: Vec<Object>) -> Object {
    if let Object::Function(function_object) = function {
        let mut extended_env = object::Environment::new_enclosed(function_object.env.clone());
        for (param, arg) in function_object.function_literal.parameters.iter().zip(args) {
            extended_env.set(param.0.clone(), arg);
        }
        let evaluated = eval_block_statement(
            &function_object.function_literal.body,
            Rc::new(RefCell::new(extended_env)),
        );
        return unwrap_return_value(evaluated);
    }
    if let Object::Builtin(built) = function {
        match built {
            Builtins::Len => {
                if args.len() > 1 {
                    return Object::Error(vec![format!(
                        "too many arguments provided for {}",
                        built
                    )]);
                }
                if args.is_empty() {
                    return Object::Error(vec![format!(
                        "too few arguments provided for {}",
                        built
                    )]);
                }
                let arg = &args[0];
                return match arg {
                    Object::StringLiteral(s) => Object::Integer(s.len() as i128),
                    _ => Object::Error(vec![format!(
                        "argument mismatch object docent have len{}",
                        built
                    )]),
                };
            }
        }
    }
    Object::Error(vec![format!(
        "not something that can be called: {}",
        function
    )])
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue(value) = obj {
        *value
    } else {
        obj
    }
}

fn eval_prefix_expression(operator: &Token, right: Object) -> Object {
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
        _ => return Object::Error(vec![format!("Invalid type for ! operator: {}", right)]),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(n) => Object::Integer(-n),
        Object::Float(n) => Object::Float(-n),
        _ => Object::Error(vec![format!("Invalid type for - operator: {}", right)]),
    }
}

fn eval_bitwise_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(n) => Object::Integer(!n),
        Object::Float(n) => Object::Integer(!(n as i128)),
        Object::Boolean(b) => Object::Boolean(!b),
        _ => Object::Error(vec![format!("Invalid type for ~ operator: {}", right)]),
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
                    } else {
                        (c as u8 + 1) as char // Increment ASCII value and convert back to char
                    }
                })
                .collect();
            Object::StringLiteral(new_s)
        }
        _ => Object::Error(vec![format!("Invalid type for ++ operator: {}", right)]),
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
                    } else {
                        (c as u8 - 1) as char // Increment ASCII value and convert back to char
                    }
                })
                .collect();
            Object::StringLiteral(new_s)
        }
        _ => Object::Error(vec![format!("Invalid type for -- operator: {}", right)]),
    }
}

fn eval_infix_expression(left: Object, operator: &Token, right: Object) -> Object {
    match operator {
        Token::Add => eval_plus_infix_expression(left, right),
        Token::Sub => eval_minus_infix_expression(left, right),
        Token::Mul => eval_multiply_infix_expression(left, right),
        Token::Power => eval_power_infix_expression(left, right),
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
        _ => Object::Error(vec![format!(
            "Invalid infix operator: {} {} {}",
            left, operator, right
        )]),
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

fn eval_plus_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l + r),
        (Object::Float(l), Object::Float(r)) => Object::Float(l + r),
        (Object::Integer(l), Object::Float(r)) => Object::Float(l as f64 + r),
        (Object::Float(l), Object::Integer(r)) => Object::Float(l + r as f64),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::StringLiteral(l + &r),
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::Add
        )]),
    }
}

fn eval_minus_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l - r),
        (Object::Float(l), Object::Float(r)) => Object::Float(l - r),
        (Object::Integer(l), Object::Float(r)) => Object::Float(l as f64 - r),
        (Object::Float(l), Object::Integer(r)) => Object::Float(l - r as f64),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => {
            Object::StringLiteral(l.replace(&r, ""))
        }
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::Sub
        )]),
    }
}

fn eval_multiply_infix_expression(left: Object, right: Object) -> Object {
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
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::Mul
        )]),
    }
}

fn eval_divide_infix_expression(left: Object, right: Object) -> Object {
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
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::Div
        )]),
    }
}

fn eval_modulo_infix_expression(left: Object, right: Object) -> Object {
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
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::Mod
        )]),
    }
}

fn eval_bitwise_and_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l & r),
        (Object::Float(l), Object::Float(r)) => Object::Integer(l as i128 & r as i128),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l & r),
        (Object::Integer(l), Object::Boolean(r)) => Object::Integer(l & r as i128),
        (Object::Boolean(l), Object::Integer(r)) => Object::Integer(l as i128 & r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => {
            Object::StringLiteral(l.chars().filter(|c| r.contains(*c)).collect())
        }
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::BitwiseAnd
        )]),
    }
}

fn eval_bitwise_or_infix_expression(left: Object, right: Object) -> Object {
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
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::BitwiseOr
        )]),
    }
}

fn eval_bitwise_xor_infix_expression(left: Object, right: Object) -> Object {
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
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::BitwiseXor
        )]),
    }
}

fn eval_bitwise_left_shift_infix_expression(left: Object, right: Object) -> Object {
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
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::ShiftLeft
        )]),
    }
}

fn eval_bitwise_right_shift_infix_expression(left: Object, right: Object) -> Object {
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
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::ShiftRight
        )]),
    }
}

fn eval_equal_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l == r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l == r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l == r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l == r),
        _ => Object::Boolean(false),
    }
}

fn eval_not_equal_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l != r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l != r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l != r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l != r),
        _ => Object::Boolean(true),
    }
}

fn eval_less_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l < r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l < r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l < r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l < r),
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::Less
        )]),
    }
}

fn eval_less_eq_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l <= r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l <= r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l <= r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l <= r),
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::LessEq
        )]),
    }
}

fn eval_greater_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l > r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l > r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l > r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l > r),
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::Greater
        )]),
    }
}

fn eval_greater_eq_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Boolean(l >= r),
        (Object::Float(l), Object::Float(r)) => Object::Boolean(l >= r),
        (Object::Boolean(l), Object::Boolean(r)) => Object::Boolean(l >= r),
        (Object::StringLiteral(l), Object::StringLiteral(r)) => Object::Boolean(l >= r),
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::GreaterEq
        )]),
    }
}

fn eval_postfix_expression(operator: &Token, right: Object) -> Object {
    match operator {
        Token::Increment => eval_increment_operator_expression(right),
        Token::Decrement => eval_decrement_operator_expression(right),
        o => Object::Error(vec![format!("Invalid postfix operator: {}{}", right, o)]),
    }
}

fn eval_power_infix_expression(left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => Object::Integer(l.pow(r as u32)),
        (Object::Float(l), Object::Float(r)) => Object::Float(l.powf(r)),
        (Object::Integer(l), Object::Float(r)) => Object::Float((l as f64).powf(r)),
        (Object::Float(l), Object::Integer(r)) => Object::Float(l.powf(r as f64)),
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {token} operator: {} {token} {}",
            l,
            r,
            token = Token::Power
        )]),
    }
}

fn eval_let_statement(
    let_statement: &ast::LetStatement,
    env: Rc<RefCell<object::Environment>>,
) -> Object {
    let value = eval_expression(&let_statement.value, env.clone());
    env.borrow_mut().set(let_statement.name.0.clone(), value);
    NULL_OBJ
}

fn eval_identifier(node: &ast::Identifier, env: Rc<RefCell<object::Environment>>) -> Object {
    match node.try_into() as Result<object::Builtins, ()> {
        Ok(b) => Object::Builtin(b),
        Err(_) => match env.borrow().get(&node.0) {
            Some(obj) => obj,
            None => Object::Error(vec![format!("Identifier not found: {}", node.0)]),
        },
    }
}

fn eval_if_expression(
    if_expression: &ast::IfExpression,
    env: Rc<RefCell<object::Environment>>,
) -> Object {
    let condition = eval_expression(&if_expression.condition, env.clone());
    if is_truthy(condition) {
        return eval_block_statement(&if_expression.consequence, env);
    } else if let Some(else_clause) = &if_expression.alternative {
        return eval_block_statement(else_clause, env);
    } else {
        return NULL_OBJ;
    }
}


fn eval_array_expression(array_expression: &ast::ArrayLiteral, env: Rc<RefCell<object::Environment>>) -> Object {
    let elements = array_expression.elements.iter().map(|e| eval_expression(e, env.clone())).collect();
    Object::Array(elements)
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    match (left, index) {
        (Object::Array(elements), Object::Integer(i)) => {
            if i < 0 || i >= elements.len() as i128 {
                return Object::Error(vec![format!("Index out of bounds: {}", i)]);
            }
            elements[i as usize].clone()
        }
        (Object::Array(elements), Object::Float(i)) => {
            if i < 0.0 || i >= elements.len() as f64 {
                return Object::Error(vec![format!("Index out of bounds: {}", i)]);
            }
            elements[i as usize].clone()
        }
        (Object::StringLiteral(s), Object::Integer(i)) => {
            if i < 0 || i >= s.len() as i128 {
                return Object::Error(vec![format!("Index out of bounds: {}", i)]);
            }
            Object::StringLiteral(s[i as usize..].to_string())
        }
        (Object::StringLiteral(s), Object::Float(i)) => {
            if i < 0.0 || i >= s.len() as f64 {
                return Object::Error(vec![format!("Index out of bounds: {}", i)]);
            }
            Object::StringLiteral(s[i as usize..].to_string())
        }
        (l, r) => Object::Error(vec![format!(
            "Invalid types for {t1} {t2} operator: {} {t1} {} {t2}",
            l,
            r,
            t1 = Token::LBracket,
            t2 = Token::RBracket
        )]),
    }
}