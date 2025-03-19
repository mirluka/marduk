use crate::lexer::{Keyword, Token};
use std::error::Error;
use std::fmt::Display;

macro_rules! expect_token {
    ($($name: ident: $token_type: path, $expected: ident), *) => {
        $(
            fn $name<'a>(&self, token: &'a Token, message: &'static str) -> Result<&'a $expected, AstError<'a>> {
                if let $token_type(val) = token {
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
    UnexpectedToken(&'static str, &'a Token),
}

impl<'a> Display for AstError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstError::UnexpectedToken(msg, token) => write!(f, "{}, found {}", msg, token),
            AstError::EmptyTree => write!(f, "Empty syntax tree"),
        }
    }
}

impl<'a> Error for AstError<'a> {}

#[derive(Debug, PartialEq, Eq)]
enum Directive {
    Get,
}

struct DirectiveTree {
    directive_type: Directive,
    class_name: String,
}

impl DirectiveTree {
    pub fn new(directive_type: Directive, class_name: String) -> Self {
        Self {
            directive_type,
            class_name,
        }
    }

    pub fn class_name(&self) -> &String {
        &self.class_name
    }

    pub fn directive_type(&self) -> &Directive {
        &self.directive_type
    }
}

#[derive(Debug)]
struct MarqlAst {
    tokens: Vec<Token>,
}

impl MarqlAst {
    pub fn new<'a>(tokens: Vec<Token>) -> Result<MarqlAst, AstError<'a>> {
        if tokens.is_empty() || tokens.len() < 2 {
            return Err(AstError::EmptyTree);
        }

        Ok(Self { tokens })
    }

    pub fn build_directive_tree(&self) -> Result<DirectiveTree, AstError> {
        let first_token = &self.tokens[0];
        let directive_type_keyword =
            self.expect_keyword(first_token, "Expected directive (GET)")?;

        let directive_type = match directive_type_keyword {
            Keyword::Get => Directive::Get,
            _ => {
                return Err(AstError::UnexpectedToken(
                    "Expected directive (GET)",
                    first_token,
                ));
            }
        };

        let class_name = self.expect_word(&self.tokens[1], "")?.to_owned();

        Ok(DirectiveTree::new(directive_type, class_name))
    }

    expect_token! {
        expect_keyword: Token::Keyword, Keyword,
        expect_word: Token::Word, String
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Keyword;

    use super::*;

    #[test]
    fn fail_when_no_tokens_provided() {
        let tokens = Vec::new();
        let ast_error = MarqlAst::new(tokens).unwrap_err();

        assert_eq!(ast_error, AstError::EmptyTree)
    }

    #[test]
    fn fail_when_a_single_token_provided() {
        let tokens = vec![Token::Keyword(Keyword::Get)];
        let ast_error = MarqlAst::new(tokens).unwrap_err();

        assert_eq!(ast_error, AstError::EmptyTree)
    }

    #[test]
    fn generate_ast_with_directive_type_and_name() {
        let tokens = vec![
            Token::Keyword(Keyword::Get),
            Token::Word("products".to_string()),
        ];
        let ast = MarqlAst::new(tokens).unwrap();

        let directive_tree = ast.build_directive_tree().unwrap();

        assert_eq!(directive_tree.directive_type(), &Directive::Get);
        assert_eq!(directive_tree.class_name(), &"products".to_string());
    }
}
