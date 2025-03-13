use phf::phf_map;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Keyword {
    Get,
}

const KEYWORDS_MAP: phf::Map<&'static str, Keyword> = phf_map! {
    "get" => Keyword::Get
};

const SINGLE_CHAR_OPERATORS: &[char] = &[',', '(', ')'];

#[derive(Debug)]
enum LexerError {
    EmptyQuery,
    UnknownCharacter(char),
}

impl std::error::Error for LexerError {}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::EmptyQuery => write!(f, "Empty Marduk query"),
            LexerError::UnknownCharacter(ch) => write!(f, "Unknown character in the query: {}", ch),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Token {
    Word(String),
    OpenParantheses,
    CloseParantheses,
    Comma,
    Keyword(Keyword),
}

struct Lexer {
    query_str: String,
}

impl Lexer {
    pub fn new(query: String) -> Result<Self, LexerError> {
        if query.is_empty() {
            return Err(LexerError::EmptyQuery);
        }

        Ok(Self { query_str: query })
    }

    pub fn generate_tokens(&self) -> Result<Vec<Token>, LexerError> {
        let chars: Vec<char> = self.query_str.chars().collect();
        let mut tokens = Vec::with_capacity(10);
        let mut offset = 0;

        while offset < chars.len() {
            let ch = chars[offset];
            offset += 1;
            // If the character we read is empty, new line or tab then continue with the next character
            if ch == ' ' || self.is_escape_character(ch) {
                continue;
            }

            let token = match ch {
                'a'..'z' => self.parse_word_or_keyword(ch, &chars, &mut offset)?,
                '(' => Token::OpenParantheses,
                ')' => Token::CloseParantheses,
                ',' => Token::Comma,
                _ => return Err(LexerError::UnknownCharacter(ch)),
            };

            tokens.push(token);
        }

        Ok(tokens)
    }

    fn parse_word_or_keyword(
        &self,
        read_char: char,
        chars: &[char],
        offset: &mut usize,
    ) -> Result<Token, LexerError> {
        let mut word_vector = Vec::with_capacity(30);

        word_vector.push(read_char);

        while *offset < chars.len() {
            let ch = chars[*offset];

            if self.is_escape_character(ch) {
                continue;
            }

            if ch == ' ' || SINGLE_CHAR_OPERATORS.contains(&ch) {
                break;
            }

            *offset += 1;
            word_vector.push(ch);
        }

        let word = String::from_iter(word_vector);

        let token = KEYWORDS_MAP
            .get(word.as_str())
            .map(|keyword| Token::Keyword(keyword.to_owned()))
            .unwrap_or(Token::Word(word));

        Ok(token)
    }

    fn is_escape_character(&self, ch: char) -> bool {
        matches!(ch, '\n' | '\r' | '\t' | '\\' | '\x08' | '\x0C')
    }
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn parse_get_query_with_object_name() {
        let test_query: &str = "get process_az(name, start_time)";

        let lexer = Lexer::new(test_query.to_string()).unwrap();

        let expected_tokens = vec![
            Token::Keyword(Keyword::Get),
            Token::Word("process_az".to_string()),
            Token::OpenParantheses,
            Token::Word("name".to_string()),
            Token::Comma,
            Token::Word("start_time".to_string()),
            Token::CloseParantheses,
        ];

        match lexer.generate_tokens() {
            Ok(tokens) => assert_eq!(tokens, expected_tokens),
            Err(err) => panic!("Test failed with error: {:?}", err),
        }
    }
}
