use crate::lexer::{Keyword, Operator, Token};
use std::cell::Cell;
use std::error::Error;
use std::fmt::Display;

macro_rules! expect_token {
    ($($name: ident: $token_type: path, $expected: ident), *) => {
        $(
            fn $name(&self, message: &'static str) -> Result<&$expected, AstError> {
                let offset = self.offset.get();
                let token = &self.tokens.get(offset).ok_or(AstError::UnexpectedEndOfTokens)?;

                if let $token_type(val) = token {
                    self.offset.set(offset + 1);
                    return Ok(val)
                }
                else {
                    return Err(AstError::UnexpectedToken(message, token))
                }
            }
        )*
    }

}

#[derive(Debug, PartialEq, Eq)]
enum AstError<'a> {
    EmptyTree,
    NoDirective,
    UnexpectedEndOfTokens,
    EmptyPickExpression,
    UnexpectedToken(&'static str, &'a Token),
}

impl<'a> Display for AstError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstError::UnexpectedToken(msg, token) => write!(f, "{}, found {}", msg, token),
            AstError::NoDirective => write!(f, "No directive provided."),
            AstError::UnexpectedEndOfTokens => write!(f, "Query is not complete."),
            AstError::EmptyTree => write!(f, "Empty syntax tree"),
            AstError::EmptyPickExpression => write!(f, "Empty PICK expression definition"),
        }
    }
}

impl<'a> Error for AstError<'a> {}

#[derive(Debug, PartialEq, Eq)]
enum Directive {
    Get,
}

#[derive(Debug, PartialEq, Eq)]
enum PickAtomic {
    Identifier(String),
    String(String),
    Integer(i32),
}

#[derive(Debug, PartialEq, Eq)]
enum PickExpression {
    Atomic(PickAtomic),
    Equals(PickAtomic, PickAtomic),
    And(Box<PickExpression>, Box<PickExpression>),
    Or(Box<PickExpression>, Box<PickExpression>)
}


#[derive(Debug)]
struct DirectiveClass {
    name: String,
    fields: Option<Vec<String>>
}

#[derive(Debug)]
struct DirectiveTree {
    directive_type: Directive,
    class: DirectiveClass,
    pick: Option<PickExpression>
}

impl DirectiveTree {
    pub fn new(directive_type: Directive, class: DirectiveClass, pick: Option<PickExpression>) -> Self {
        Self {
            directive_type,
            class,
            pick
        }
    }

    pub fn class(&self) -> &DirectiveClass {
        &self.class
    }

    pub fn directive_type(&self) -> &Directive {
        &self.directive_type
    }

    pub fn pick(&self) -> &Option<PickExpression> {
        &self.pick
    }
}

#[derive(Debug)]
struct MarqlAst {
    tokens: Vec<Token>,
    offset: Cell<usize>,
}

impl MarqlAst {
    pub fn new<'a>(tokens: Vec<Token>) -> Result<MarqlAst, AstError<'a>> {
        if tokens.is_empty() || tokens.len() < 2 {
            return Err(AstError::EmptyTree);
        }

        Ok(Self {
            tokens: tokens,
            offset: Cell::new(0),
        })
    }

    pub fn build_directive_tree(&self) -> Result<DirectiveTree, AstError> {
        let directive_type_keyword = self.expect_keyword("Expected directive (GET)")?;

        let directive_type = match directive_type_keyword {
            Keyword::Get => Directive::Get,
            _ => {
                return Err(AstError::UnexpectedToken(
                    "Expected directive (GET)",
                    &self.tokens[0],
                ));
            }
        };

        let directive_class = self.expect_directive_class()?;
        let pick_expression = self.parse_pick()?;

        Ok(DirectiveTree::new(directive_type, directive_class, pick_expression))
    }

    fn expect_directive_class(&self) -> Result<DirectiveClass, AstError> {
        let token_slice = &self.tokens[self.offset.get()..];

        if token_slice.is_empty() {
            return Err(AstError::NoDirective);
        }

        let name = self.expect_word("Expected a directive name")?;
        let fields = if token_slice.len() > 1 {
            match &token_slice[1] {
                Token::Operator(op) if op == &Operator::OpenParantheses => {
                    Some(self.get_class_fields()?)
                }
                Token::Keyword(_) => None,
                _ => {
                    return Err(AstError::UnexpectedToken(
                        "Unexpected character(s) after class definition",
                        &token_slice[1],
                    ));
                }
            }
        } else {
            None
        };

        Ok(DirectiveClass {
            name: name.to_owned(),
            fields,
        })
    }

    fn get_class_fields(&self) -> Result<Vec<String>, AstError> {
        let open_parantheses_op = self.expect_operator("Expected \"(\" after directive name")?;

        if open_parantheses_op != &Operator::OpenParantheses {
            return Err(AstError::UnexpectedToken(
                "Expected \"(\" after directive name",
                &self.tokens[self.offset.get()],
            ));
        }

        let mut fields = Vec::with_capacity(10);
        let mut fields_collected = false;

        while self.offset.get() < self.tokens.len() {
            let field_name = self.expect_word("Expected directive field")?;
            let comma_or_parantheses_op =
                self.expect_operator("Expected comma (,) or closed parantheses \")\"")?;

            //If the fields are being closed then exit the loop.
            if comma_or_parantheses_op == &Operator::CloseParantheses {
                fields.push(field_name.to_owned());
                fields_collected = true;
                break;
            }

            if comma_or_parantheses_op != &Operator::Comma {
                return Err(AstError::UnexpectedToken(
                    "Expected comma (,) or closed parantheses \")\"",
                    &self.tokens[self.offset.get() - 1],
                ));
            }

            fields.push(field_name.to_owned());
        }

        if !fields_collected {
            return Err(AstError::UnexpectedEndOfTokens);
        }

        Ok(fields)
    }

    fn parse_pick(&self) -> Result<Option<PickExpression>, AstError> {
        let offset = self.offset.get();

        if self.offset.get() == self.tokens.len() {
            return Ok(None);
        }

        let token = &self.tokens[self.offset.get() as usize];

        if let Token::Keyword(Keyword::Pick) = token {
            let new_offset = offset + 1;
            self.offset.set(new_offset);

            // Check if we have more tokens after the pick keyword. if not, then return an error
            // with UnexpextedEndOfTokens
            if new_offset == self.tokens.len() {
                return Err(AstError::UnexpectedEndOfTokens);
            } else {
                let identifier_stack = Vec::with_capacity(10);
                return self.parse_pick_expression(identifier_stack).map(|exp| Some(exp));
            }
        } else {
            return Ok(None)
        }

    }

    fn parse_pick_expression(&self, mut expression_stack: Vec<PickExpression>) -> Result<PickExpression, AstError> {

        while let Some(token_ref) = self.tokens.get(self.offset.get() as usize) {

            match token_ref {
                Token::Word(word) => {
                    let expression = self.parse_pick_atomic_identifier(&mut expression_stack, word)?;
                    expression_stack.push(expression);
                },
                Token::Operator(operator) => todo!(),
                Token::Keyword(keyword) => todo!(),
                Token::String(string) => {
                    let expression = self.parse_pick_atomic_string(&mut expression_stack, string)?;
                    expression_stack.push(expression);
                },
                Token::Integer(integer) => {
                    let expression = self.parse_pick_atomic_integer(&mut expression_stack, integer.clone())?;
                    expression_stack.push(expression);
                },
            }

        }

        //TODO: Check cases when identifier_stack has more than 1 element

        match expression_stack.pop() {
            Some(pick_expression) => Ok(pick_expression),
            None => Err(AstError::EmptyPickExpression),
        }
    }

    fn parse_pick_atomic_identifier(&self, expression_stack: &mut Vec<PickExpression>, word: &String) -> Result<PickExpression, AstError> {
       let last_expression =  expression_stack.last();
       let token = &self.tokens[self.offset.get()];
       self.offset.set(self.offset.get() + 1);

       match last_expression {
           Some(exp) => {
               if let PickExpression::Atomic(_) = exp {
                   Err(AstError::UnexpectedToken("Identifiers can only be used with operators", token))
               } else {
                   Ok(PickExpression::Atomic(PickAtomic::Identifier(word.to_string()))) 
               }
           }
           None => Ok(PickExpression::Atomic(PickAtomic::Identifier(word.to_string())))
       }

    }

    fn parse_pick_atomic_string(&self, expression_stack: &mut Vec<PickExpression>, word: &String) -> Result<PickExpression, AstError> {
       let last_expression =  expression_stack.last();
       let token = &self.tokens[self.offset.get()];
       self.offset.set(self.offset.get() + 1);

       match last_expression {
           Some(exp) => {
               if let PickExpression::Atomic(_) = exp {
                   Err(AstError::UnexpectedToken("Strings can only be used with operators", token))
               } else {
                   Ok(PickExpression::Atomic(PickAtomic::String(word.to_string()))) 
               }
           }
           None => Err(AstError::UnexpectedToken("Strings can only be used with operators", token))
       }

    }

    fn parse_pick_atomic_integer(&self, expression_stack: &mut Vec<PickExpression>, int: i32) -> Result<PickExpression, AstError> {
       let last_expression =  expression_stack.last();
       let token = &self.tokens[self.offset.get()];
       self.offset.set(self.offset.get() + 1);

       match last_expression {
           Some(exp) => {
               if let PickExpression::Atomic(_) = exp {
                   Err(AstError::UnexpectedToken("Integers can only be used with operators", token))
               } else {
                   Ok(PickExpression::Atomic(PickAtomic::Integer(int))) 
               }
           }
           None => Err(AstError::UnexpectedToken("Integers can only be used with operators", token))
       }

    }

    expect_token! {
        expect_keyword: Token::Keyword, Keyword,
        expect_word: Token::Word, String,
        expect_operator: Token::Operator, Operator
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Keyword;

    use super::*;

    macro_rules! ast_init_test_failure {
        ($($name: ident: $tokens: expr => $expected: expr), *) => {
            $(
                #[test]
                fn $name() {
                    let ast_error = MarqlAst::new($tokens).unwrap_err();
                    assert_eq!(ast_error, $expected)
                }
            )*
        };
    }

    macro_rules! ast_test_failure {
        ($($name: ident: $tokens: expr => $expected: expr), *) => {
            $(
                #[test]
                fn $name() {
                    let ast = MarqlAst::new($tokens).unwrap();
                    let err = ast.build_directive_tree().unwrap_err();
                    assert_eq!(err, $expected)
                }
            )*
        };
    }

    ast_init_test_failure! {
        fail_when_no_tokens_provided: Vec::new() => AstError::EmptyTree,
        fail_when_a_single_token_provided: vec![Token::Keyword(Keyword::Get)] => AstError::EmptyTree
    }

    ast_test_failure! {
        fail_when_directive_field_parantheses_open_but_not_closed: vec![
            Token::Keyword(Keyword::Get),
            Token::Word("test".to_string()),
            Token::Operator(Operator::OpenParantheses)
        ] => AstError::UnexpectedEndOfTokens,
        fail_when_directive_field_parantheses_with_non_open_parantheses: vec![
            Token::Keyword(Keyword::Get),
            Token::Word("test".to_string()),
            Token::Operator(Operator::CloseParantheses)
        ] => AstError::UnexpectedToken("Unexpected character(s) after class definition", &Token::Operator(Operator::CloseParantheses)),
        fail_when_directive_field_with_no_parantheses: vec![
            Token::Keyword(Keyword::Get),
            Token::Word("test".to_string()),
            Token::Word("hello".to_string())
        ] => AstError::UnexpectedToken("Unexpected character(s) after class definition", &Token::Word("hello".to_string())),
        fail_when_directive_field_with_no_closed_parantheses: vec![
            Token::Keyword(Keyword::Get),
            Token::Word("test".to_string()),
            Token::Operator(Operator::OpenParantheses),
            Token::Word("field".to_string())
        ] => AstError::UnexpectedEndOfTokens,
        fail_when_directive_field_with_no_closed_parantheses_2: vec![
            Token::Keyword(Keyword::Get),
            Token::Word("test".to_string()),
            Token::Operator(Operator::OpenParantheses),
            Token::Word("field".to_string()),
            Token::Operator(Operator::Comma)
        ] => AstError::UnexpectedEndOfTokens,
        fail_when_directive_field_with_no_comma_operator: vec![
            Token::Keyword(Keyword::Get),
            Token::Word("test".to_string()),
            Token::Operator(Operator::OpenParantheses),
            Token::Word("field".to_string()),
            Token::Operator(Operator::Equals)
        ] => AstError::UnexpectedToken("Expected comma (,) or closed parantheses \")\"", &Token::Operator(Operator::Equals)),
        fail_when_directive_field_not_closing_with_parantheses: vec![
            Token::Keyword(Keyword::Get),
            Token::Word("products".to_string()),
            Token::Operator(Operator::OpenParantheses),
            Token::Word("field1".to_string()),
            Token::Operator(Operator::Comma),
            Token::Word("field2".to_string()),
            Token::Operator(Operator::Equals),
        ] => AstError::UnexpectedToken("Expected comma (,) or closed parantheses \")\"", &Token::Operator(Operator::Equals))
    }

    #[test]
    fn generate_ast_with_directive_type_and_name() {
        let tokens = vec![
            Token::Keyword(Keyword::Get),
            Token::Word("products".to_string()),
        ];
        let tokens_len = tokens.len();
        let ast = MarqlAst::new(tokens).unwrap();

        let directive_tree = ast.build_directive_tree().unwrap();

        assert_eq!(directive_tree.directive_type(), &Directive::Get);
        assert_eq!(directive_tree.class().name, "products".to_string());
        assert_eq!(directive_tree.class().fields, None);
        assert_eq!(ast.offset.get(), tokens_len);
    }

    #[test]
    fn generate_ast_with_directive_type_and_name_with_multiple_fields() {
        let tokens = vec![
            Token::Keyword(Keyword::Get),
            Token::Word("products".to_string()),
            Token::Operator(Operator::OpenParantheses),
            Token::Word("field1".to_string()),
            Token::Operator(Operator::Comma),
            Token::Word("field2".to_string()),
            Token::Operator(Operator::CloseParantheses),
        ];
        let tokens_len = tokens.len();
        let ast = MarqlAst::new(tokens).unwrap();

        let directive_tree = ast.build_directive_tree().unwrap();

        assert_eq!(directive_tree.directive_type(), &Directive::Get);
        assert_eq!(directive_tree.class().name, "products".to_string());
        assert_eq!(
            directive_tree.class().fields,
            Some(vec!["field1".to_string(), "field2".to_string()])
        );
        assert_eq!(ast.offset.get(), tokens_len);
    }

    #[test]
    fn generate_ast_with_directive_type_and_name_with_single_fields() {
        let tokens = vec![
            Token::Keyword(Keyword::Get),
            Token::Word("products".to_string()),
            Token::Operator(Operator::OpenParantheses),
            Token::Word("field1".to_string()),
            Token::Operator(Operator::CloseParantheses),
        ];
        let tokens_len = tokens.len();
        let ast = MarqlAst::new(tokens).unwrap();

        let directive_tree = ast.build_directive_tree().unwrap();

        assert_eq!(directive_tree.directive_type(), &Directive::Get);
        assert_eq!(directive_tree.class().name, "products".to_string());
        assert_eq!(
            directive_tree.class().fields,
            Some(vec!["field1".to_string()])
        );
        assert_eq!(ast.offset.get(), tokens_len);
    }

    #[test]
    fn generate_ast_with_basic_pick_fields() {
        let tokens = vec![
            Token::Keyword(Keyword::Get),
            Token::Word("products".to_string()),
            Token::Keyword(Keyword::Pick),
            Token::Word("valid".to_string()),
        ];
        let tokens_len = tokens.len();
        let ast = MarqlAst::new(tokens).unwrap();

        let directive_tree = ast.build_directive_tree().unwrap();

        assert_eq!(directive_tree.directive_type(), &Directive::Get);
        assert_eq!(directive_tree.class().name, "products".to_string());
        assert_eq!(directive_tree.class().fields, None);
        assert_eq!(directive_tree.pick().as_ref().unwrap(), &PickExpression::Atomic(PickAtomic::Identifier("valid".to_string())));
        assert_eq!(ast.offset.get(), tokens_len);
    }

}
