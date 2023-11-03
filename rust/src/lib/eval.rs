use super::object::ObjectType;

use super::ast;
use super::ast::Node;
use super::object;
use super::token;

pub fn eval(node: &dyn ast::Expression) -> Box<dyn object::Object> {
    match node.token() {
        token::Token::INT(i) => Box::new(object::Integer { value: *i }) as Box<dyn object::Object>,
        token::Token::FLOAT(f) => Box::new(object::Float { value: *f }) as Box<dyn object::Object>,
        token::Token::STRING(ref s) => {
            Box::new(object::StringObj { value: s.clone() }) as Box<dyn object::Object>
        }
        token::Token::TRUE => Box::new(object::Boolean { value: true }) as Box<dyn object::Object>,
        token::Token::FALSE => {
            Box::new(object::Boolean { value: false }) as Box<dyn object::Object>
        }
        token::Token::BANG => {
            let prefix = node.into_prefix_expression().unwrap();
            eval_bang_operator_expression(prefix) as Box<dyn object::Object>
        }
        token::Token::MINUS if node.into_prefix_expression().is_some() => {
            let Some(prefix) = node.into_prefix_expression() else {
                return Box::new(object::Null);
            };
            eval_minus_prefix_operator_expression(prefix) as Box<dyn object::Object>
        }
        token::Token::PLUS
        | token::Token::ASTRISK
        | token::Token::SLASH
        | token::Token::EOF
        | token::Token::MINUS
        | token::Token::MOD
        | token::Token::NOTEQ
        | token::Token::EQ
        | token::Token::LT
        | token::Token::GT
        | token::Token::LTEQ
        | token::Token::GTEQ => {
            let Some(infix) = node.into_infix_expression() else {
                return Box::new(object::Null);
            };
            eval_infix_expression(&infix.token, infix.left.as_ref(), infix.right.as_ref())
                as Box<dyn object::Object>
        }
        _ => Box::new(object::Null),
    }
}

fn eval_infix_expression(
    operator: &token::Token,
    left: &dyn ast::Expression,
    right: &dyn ast::Expression,
) -> Box<dyn object::Object> {
    let left = eval(left);
    let right = eval(right);
    match (left.object_type(), right.object_type()) {
        (ObjectType::Boolean(l), ObjectType::Boolean(r)) => match operator {
            token::Token::EQ => Box::new(object::Boolean { value: l == r }),
            token::Token::NOTEQ => Box::new(object::Boolean { value: l != r }),
            _ => Box::new(object::Null),
        },
        (ObjectType::Integer(l), ObjectType::Integer(r)) => match operator {
            token::Token::EQ => Box::new(object::Boolean { value: l == r }),
            token::Token::NOTEQ => Box::new(object::Boolean { value: l != r }),
            token::Token::LT => Box::new(object::Boolean { value: l < r }),
            token::Token::GT => Box::new(object::Boolean { value: l > r }),
            token::Token::LTEQ => Box::new(object::Boolean { value: l <= r }),
            token::Token::GTEQ => Box::new(object::Boolean { value: l >= r }),
            _ => Box::new(object::Null),
        },
        (ObjectType::Float(l), ObjectType::Float(r)) => match operator {
            token::Token::EQ => Box::new(object::Boolean { value: l == r }),
            token::Token::NOTEQ => Box::new(object::Boolean { value: l != r }),
            token::Token::LT => Box::new(object::Boolean { value: l < r }),
            token::Token::GT => Box::new(object::Boolean { value: l > r }),
            token::Token::LTEQ => Box::new(object::Boolean { value: l <= r }),
            token::Token::GTEQ => Box::new(object::Boolean { value: l >= r }),
            _ => Box::new(object::Null),
        },
        (ObjectType::Integer(l), ObjectType::Float(r)) => match operator {
            token::Token::EQ => Box::new(object::Boolean {
                value: l as f64 == r,
            }),
            token::Token::NOTEQ => Box::new(object::Boolean {
                value: l as f64 != r,
            }),
            token::Token::LT => Box::new(object::Boolean {
                value: (l as f64) < r,
            }),
            token::Token::GT => Box::new(object::Boolean {
                value: l as f64 > r,
            }),
            token::Token::LTEQ => Box::new(object::Boolean {
                value: l as f64 <= r,
            }),
            token::Token::GTEQ => Box::new(object::Boolean {
                value: l as f64 >= r,
            }),
            _ => Box::new(object::Null),
        },
        (ObjectType::Float(l), ObjectType::Integer(r)) => match operator {
            token::Token::EQ => Box::new(object::Boolean {
                value: l == r as f64,
            }),
            token::Token::NOTEQ => Box::new(object::Boolean {
                value: l != r as f64,
            }),
            token::Token::LT => Box::new(object::Boolean {
                value: l < r as f64,
            }),
            token::Token::GT => Box::new(object::Boolean {
                value: l > r as f64,
            }),
            token::Token::LTEQ => Box::new(object::Boolean {
                value: l <= r as f64,
            }),
            token::Token::GTEQ => Box::new(object::Boolean {
                value: l >= r as f64,
            }),
            _ => Box::new(object::Null),
        },
        (ObjectType::StringObj(l), ObjectType::StringObj(r)) => match operator {
            token::Token::EQ => Box::new(object::Boolean { value: l == r }),
            token::Token::NOTEQ => Box::new(object::Boolean { value: l != r }),
            token::Token::LT => Box::new(object::Boolean { value: l < r }),
            token::Token::GT => Box::new(object::Boolean { value: l > r }),
            token::Token::LTEQ => Box::new(object::Boolean { value: l <= r }),
            token::Token::GTEQ => Box::new(object::Boolean { value: l >= r }),
            _ => Box::new(object::Null),
        },
        (ObjectType::Integer(l), ObjectType::Integer(r)) => match operator {
            token::Token::PLUS => Box::new(object::Integer { value: l + r }),
            token::Token::MINUS => Box::new(object::Integer { value: l - r }),
            token::Token::ASTRISK => Box::new(object::Integer { value: l * r }),

            token::Token::SLASH => Box::new(object::Float {
                value: l as f64 / r as f64,
            }),
            token::Token::MOD => Box::new(object::Integer { value: l % r }),
            _ => Box::new(object::Null),
        },
        (object::ObjectType::Float(l), object::ObjectType::Float(r)) => match operator {
            token::Token::PLUS => Box::new(object::Float { value: l + r }),
            token::Token::MINUS => Box::new(object::Float { value: l - r }),
            token::Token::ASTRISK => Box::new(object::Float { value: l * r }),
            token::Token::SLASH => Box::new(object::Float { value: l / r }),
            token::Token::MOD => Box::new(object::Float { value: l % r }),
            _ => Box::new(object::Null),
        },
        (object::ObjectType::Integer(l), object::ObjectType::Float(r)) => match operator {
            token::Token::PLUS => Box::new(object::Float {
                value: l as f64 + r,
            }),
            token::Token::MINUS => Box::new(object::Float {
                value: l as f64 - r,
            }),
            token::Token::ASTRISK => Box::new(object::Float {
                value: l as f64 * r,
            }),
            token::Token::SLASH => Box::new(object::Float {
                value: l as f64 / r,
            }),
            token::Token::MOD => Box::new(object::Float {
                value: l as f64 % r,
            }),
            _ => Box::new(object::Null),
        },
        (object::ObjectType::Float(l), object::ObjectType::Integer(r)) => match operator {
            token::Token::PLUS => Box::new(object::Float {
                value: l + r as f64,
            }),
            token::Token::MINUS => Box::new(object::Float {
                value: l - r as f64,
            }),
            token::Token::ASTRISK => Box::new(object::Float {
                value: l * r as f64,
            }),
            token::Token::SLASH => Box::new(object::Float {
                value: l / r as f64,
            }),
            token::Token::MOD => Box::new(object::Float {
                value: l % r as f64,
            }),
            _ => Box::new(object::Null),
        },
        _ => Box::new(object::Null),
    }
}

pub fn eval_statements(stmts: &Vec<Box<dyn ast::Statement>>) -> Box<dyn object::Object> {
    let mut result: Box<dyn object::Object> = Box::new(object::Null);
    for stmt in stmts {
        result = match stmt.token() {
            token::Token::LET => eval_let_statement(stmt.into_let_statement().unwrap()),
            token::Token::RETURN => eval_return_statement(stmt.into_return_statement().unwrap()),
            token::Token::LBRACE => eval_block_statement(stmt.into_block_statement().unwrap()),
            token::Token::BANG | token::Token::MINUS
                if stmt
                    .into_expresion_statement()
                    .unwrap()
                    .expression
                    .into_prefix_expression()
                    .is_some() =>
            {
                eval_prefix_expression(
                    stmt.into_expresion_statement()
                        .unwrap()
                        .expression
                        .into_prefix_expression()
                        .unwrap(),
                )
            }
            token::Token::PLUS
            | token::Token::ASTRISK
            | token::Token::SLASH
            | token::Token::EOF
            | token::Token::MINUS
            | token::Token::MOD
            | token::Token::NOTEQ
            | token::Token::EQ
            | token::Token::LT
            | token::Token::GT
            | token::Token::LTEQ
            | token::Token::GTEQ => {
                let infix = stmt
                    .into_expresion_statement()
                    .unwrap()
                    .expression
                    .into_infix_expression()
                    .unwrap();
                eval_infix_expression(&infix.token, infix.left.as_ref(), infix.right.as_ref())
            }

            _ => eval_exprassion_statement(stmt.into_expresion_statement().unwrap()),
        }
    }
    result
}

fn eval_let_statement(stmt: &ast::LetStatement) -> Box<dyn object::Object> {
    eval(stmt.value.as_ref())
}

fn eval_return_statement(stmt: &ast::ReturnStatement) -> Box<dyn object::Object> {
    eval(stmt.value.as_ref())
}

fn eval_exprassion_statement(stmt: &ast::ExpressionStatement) -> Box<dyn object::Object> {
    eval(stmt.expression.as_ref())
}

fn eval_block_statement(stmt: &ast::BlockStatement) -> Box<dyn object::Object> {
    eval_statements(&stmt.statements)
}

fn eval_prefix_expression(stmt: &ast::PrefixExpression) -> Box<dyn object::Object> {
    match stmt.token {
        token::Token::BANG => eval_bang_operator_expression(stmt),
        token::Token::MINUS => eval_minus_prefix_operator_expression(stmt),
        _ => Box::new(object::Null),
    }
}

fn eval_minus_prefix_operator_expression(
    prefix: &ast::PrefixExpression,
) -> Box<dyn object::Object> {
    let right = eval(prefix.right.as_ref());
    match right.object_type() {
        object::ObjectType::Integer(i) => Box::new(object::Integer { value: -i }),
        object::ObjectType::Float(f) => Box::new(object::Float { value: -f }),
        _ => Box::new(object::Null),
    }
}

fn eval_bang_operator_expression(prefix: &ast::PrefixExpression) -> Box<dyn object::Object> {
    let TRUE = Box::new(object::Boolean { value: true });
    let FALSE = Box::new(object::Boolean { value: false });
    match prefix.right.token() {
        token::Token::TRUE => FALSE,
        token::Token::FALSE => TRUE,
        token::Token::INT(n) => {
            if *n > 0 {
                FALSE
            } else {
                TRUE
            }
        }
        token::Token::FLOAT(n) => {
            if *n > 0.0 {
                FALSE
            } else {
                TRUE
            }
        }
        token::Token::STRING(ref s) => {
            if !s.is_empty() {
                FALSE
            } else {
                TRUE
            }
        }
        _ => {
            let obj: Box<dyn ast::Expression> = eval(prefix.right.as_ref()).into();

            eval_bang_operator_expression(&ast::PrefixExpression {
                token: token::Token::BANG,
                right: obj,
            })
        }
    }
}

impl From<Box<dyn object::Object>> for Box<dyn ast::Expression> {
    fn from(obj: Box<dyn object::Object>) -> Self {
        match obj.object_type() {
            object::ObjectType::Integer(i) => {
                Box::new(ast::IntegerLitral(token::Token::INT(i))) as Box<dyn ast::Expression>
            }
            object::ObjectType::Float(f) => {
                Box::new(ast::FloatLitral(token::Token::FLOAT(f))) as Box<dyn ast::Expression>
            }
            object::ObjectType::Boolean(b) => Box::new(if b {
                ast::BooleanLitral(token::Token::TRUE)
            } else {
                ast::BooleanLitral(token::Token::FALSE)
            }) as Box<dyn ast::Expression>,
            object::ObjectType::StringObj(s) => {
                Box::new(ast::StringLitral(token::Token::STRING(s))) as Box<dyn ast::Expression>
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::lexer;

    use super::*;

    #[test]
    fn test_int() {
        let inputs = [(String::from("5;"), 5_i64), (String::from("10;"), 10_i64)];
        for input in inputs.iter() {
            let l = lexer::Lexer::new(input.0.clone());
            let mut p = super::super::parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval_statements(&program.statement).object_type();
            match evaluated {
                ObjectType::Integer(i) => assert_eq!(i, input.1),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_float() {
        let inputs = [
            (String::from("5.5;"), 5.5_f64),
            (String::from("10.5;"), 10.5_f64),
        ];
        for input in inputs.iter() {
            let l = lexer::Lexer::new(input.0.clone());
            let mut p = super::super::parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval_statements(&program.statement).object_type();
            match evaluated {
                ObjectType::Float(f) => assert_eq!(f, input.1),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_bool() {
        let inputs = [
            (String::from("true;"), true),
            (String::from("false;"), false),
        ];
        for input in inputs.iter() {
            let l = lexer::Lexer::new(input.0.clone());
            let mut p = super::super::parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval_statements(&program.statement).object_type();
            match evaluated {
                ObjectType::Boolean(b) => assert_eq!(b, input.1),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_string() {
        let inputs = [
            (
                String::from("\"Hello World!\""),
                String::from("Hello World!"),
            ),
            (
                String::from("\"Hello World!\";"),
                String::from("Hello World!"),
            ),
        ];
        for input in inputs.iter() {
            let l = lexer::Lexer::new(input.0.clone());
            let mut p = super::super::parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval_statements(&program.statement).object_type();
            match evaluated {
                ObjectType::StringObj(s) => assert_eq!(s, input.1),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_let_statement() {
        let inputs = [
            (String::from("let x = 5;"), 5_i64),
            (String::from("let y = 10;"), 10_i64),
        ];
        for input in inputs {
            let l = lexer::Lexer::new(input.0.clone());
            let mut p = super::super::parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval_statements(&program.statement).object_type();
            match evaluated {
                ObjectType::Integer(i) => assert_eq!(i, input.1),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_not_expression() {
        let inputs = [
            (String::from("!true"), false),
            (String::from("!false"), true),
            (String::from("!5"), false),
            (String::from("!0"), true),
            (String::from("!\"\""), true),
            (String::from("!\"a\""), false),
            (String::from("!!true"), true),
            (String::from("!!false"), false),
            (String::from("!!5"), true),
            (String::from("!!0"), false),
            (String::from("!!\"\""), false),
            (String::from("!!\"a\""), true),
        ];

        for input in inputs {
            let l = lexer::Lexer::new(input.0.clone());
            let mut p = super::super::parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval_statements(&program.statement).object_type();
            match evaluated {
                ObjectType::Boolean(b) => assert_eq!(b, input.1),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_munis_expression() {
        let inputs = [
            (String::from("-5"), -5_i64),
            (String::from("-10"), -10_i64),
            (String::from("--5"), 5_i64),
            (String::from("--10"), 10_i64),
        ];

        for input in inputs {
            let l = lexer::Lexer::new(input.0.clone());
            let mut p = super::super::parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval_statements(&program.statement).object_type();
            match evaluated {
                ObjectType::Integer(i) => assert_eq!(i, input.1),
                _ => unreachable!(),
            }
        }

        let inputs = [
            (String::from("-5.5"), -5.5_f64),
            (String::from("-10.5"), -10.5_f64),
            (String::from("--5.5"), 5.5_f64),
            (String::from("--10.5"), 10.5_f64),
        ];
        for input in inputs {
            let l = lexer::Lexer::new(input.0.clone());
            let mut p = super::super::parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval_statements(&program.statement).object_type();
            match evaluated {
                ObjectType::Float(f) => assert_eq!(f, input.1),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_infix_number_expression() {
        let inputs = [("1+1+1", 3), ("2-1-1", 0), ("2*3+1", 7)];
        for input in inputs {
            let l = lexer::Lexer::new(input.0.to_string());
            let mut p = super::super::parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval_statements(&program.statement).object_type();
            match evaluated {
                ObjectType::Integer(i) => assert_eq!(i, input.1),
                _ => unreachable!(),
            }
        }

        let inputs = [
            ("1.0+1.0+1.0", 3.0),
            ("2.0-1.0-1.0", 0.0),
            ("2/1", 2.0),
            ("2+1.0", 3.0),
        ];
        for input in inputs {
            let l = lexer::Lexer::new(input.0.to_string());
            let mut p = super::super::parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval_statements(&program.statement).object_type();
            match evaluated {
                ObjectType::Float(i) => assert_eq!(i, input.1),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_infix_logic_expression() {
        let inputs = [
            ("1==1", true),
            ("1!=1", false),
            ("1<1", false),
            ("1>1", false),
            ("1<=1", true),
            ("1>=1", true),
        ];
        for input in inputs {
            let l = lexer::Lexer::new(input.0.to_string());
            let mut p = super::super::parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval_statements(&program.statement);
            println!("{:?}", evaluated.inspect());
            match evaluated.object_type() {
                ObjectType::Boolean(i) => assert_eq!(i, input.1),
                _ => unreachable!(),
            }
        }
    }
}
