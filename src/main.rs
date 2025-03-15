use phf::phf_map;
use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Keyword {
    Get,
    Pick,
    And,
    Order,
    Asc,
    Format
}

const KEYWORDS_MAP: phf::Map<&'static str, Keyword> = phf_map! {
    "get" => Keyword::Get,
    "pick" => Keyword::Pick,
    "and" => Keyword::And,
    "order" => Keyword::Order,
    "asc" => Keyword::Asc,
    "format" => Keyword::Format,
};

const SINGLE_CHAR_OPERATORS: &[char] = &[',', '(', ')', '=', '>', '-'];

#[derive(Debug, PartialEq, Eq)]
enum LexerError {
    ExhaustedInput,
    UnknownCharacter(char),
    InvalidIdentifierCharacter(char),
    InvalidInteger(usize),
    UnknownError(usize),
}

impl std::error::Error for LexerError {}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::ExhaustedInput => write!(f, "Exhausted Input"),
            LexerError::UnknownCharacter(ch) => write!(f, "Unknown character in the query: {}", ch),
            LexerError::InvalidIdentifierCharacter(ch) => {
                write!(f, "Invalid identifier character: {}", ch)
            }
            LexerError::InvalidInteger(offset) => {
                write!(f, "Invalid integer at position {}", offset)
            }
            LexerError::UnknownError(offset) => {
                write!(f, "An unknown error occurred at position {}", offset)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Operator {
    OpenParantheses,
    CloseParantheses,
    Comma,
    Equals,
    BiggerThan,
}

#[derive(Debug, PartialEq, Eq)]
enum Token {
    Word(String),
    Operator(Operator),
    Keyword(Keyword),
    String(String),
    Integer(i32),
}

#[derive(Debug)]
struct Lexer {
    query_str: String,
}

impl Lexer {
    pub fn new(query: String) -> Result<Self, LexerError> {
        if query.is_empty() {
            return Err(LexerError::ExhaustedInput);
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
                'a'..='z' => self.parse_word_or_keyword(ch, &chars, &mut offset)?,
                'A'..='Z' => self.parse_word_or_keyword(ch, &chars, &mut offset)?,
                '0'..='9' => self.parse_integer(ch, &chars, &mut offset)?,
                // Parse negative integers
                '-' => self.parse_integer(ch, &chars, &mut offset)?,
                '(' => Token::Operator(Operator::OpenParantheses),
                ')' => Token::Operator(Operator::CloseParantheses),
                ',' => Token::Operator(Operator::Comma),
                '=' => Token::Operator(Operator::Equals),
                '>' => Token::Operator(Operator::BiggerThan),
                '"' => self.parse_string('\"', &chars, &mut offset)?,
                '\'' => self.parse_string('\'', &chars, &mut offset)?,
                _ => return Err(LexerError::UnknownCharacter(ch)),
            };

            tokens.push(token);
        }

        Ok(tokens)
    }

    fn parse_string(
        &self,
        string_start_ch: char,
        chars: &[char],
        offset: &mut usize,
    ) -> Result<Token, LexerError> {
        let mut string_completed: bool = false;
        let mut ch_vector = Vec::with_capacity(30);

        while *offset < chars.len() {
            let ch = chars[*offset];

            if ch == string_start_ch {
                string_completed = true;
                break;
            }

            ch_vector.push(ch);
            *offset += 1;
        }

        if string_completed {
            let string: String = ch_vector.into_iter().collect();
            // Increment offset by 1 to pass the closing quote
            *offset += 1;

            Ok(Token::String(string))
        } else {
            Err(LexerError::ExhaustedInput)
        }
    }

    fn parse_integer(
        &self,
        read_char: char,
        chars: &[char],
        offset: &mut usize,
    ) -> Result<Token, LexerError> {
        let mut int_vector = Vec::with_capacity(10);
        int_vector.push(read_char);

        while *offset < chars.len() {
            let ch = chars[*offset];

            if ch == ' ' {
                break;
            }

            if ch >= '0' && ch <= '9' {
                int_vector.push(ch);
            } else {
                return Err(LexerError::InvalidInteger(*offset));
            }

            *offset += 1;
        }

        if int_vector.len() == 1 && read_char == '-' {
            return Err(LexerError::InvalidInteger(*offset - 1));
        }

        let int_str: String = int_vector.into_iter().collect();

        int_str
            .parse::<i32>()
            .map_err(|_| LexerError::UnknownError(*offset))
            .map(|i| Token::Integer(i))
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

            match ch {
                '0'..='9' => word_vector.push(ch),
                'a'..='z' => word_vector.push(ch),
                'A'..='Z' => word_vector.push(ch),
                '_' => word_vector.push(ch),
                '-' => word_vector.push(ch),
                _ => return Err(LexerError::InvalidIdentifierCharacter(ch)),
            }

            *offset += 1;
        }

        let word = String::from_iter(word_vector);

        let token = KEYWORDS_MAP
            .get(word.to_lowercase().as_str())
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
    fn empty_query_should_yield_exhausted_input_error() {
        let test_query: &str = "";

        let lexer = Lexer::new(test_query.to_string()).unwrap_err();

        assert_eq!(lexer, LexerError::ExhaustedInput);
    }

    #[test]
    fn parse_word() {
        let query: &str = "process";
        let expected_tokens = vec![Token::Word("process".to_string())];

        let lexer = Lexer::new(query.to_string()).unwrap();

        match lexer.generate_tokens() {
            Ok(tokens) => assert_eq!(tokens, expected_tokens),
            Err(err) => panic!("Test failed with error: {:?}", err),
        }
    }

    #[test]
    fn fail_to_parse_unclosed_strings() {
        let query: &str = "\"ABC";

        let lexer = Lexer::new(query.to_string()).unwrap();

        let error = lexer.generate_tokens().unwrap_err();

        assert_eq!(error, LexerError::ExhaustedInput)
    }

    #[test]
    fn fail_to_parse_unclosed_strings_2() {
        let query: &str = "ABC\"";

        let lexer = Lexer::new(query.to_string()).unwrap();

        let error = lexer.generate_tokens().unwrap_err();

        assert_eq!(error, LexerError::InvalidIdentifierCharacter('"'))
    }

    #[test]
    fn fail_to_parse_unclosed_strings_with_single_quote() {
        let query: &str = "'ABC";

        let lexer = Lexer::new(query.to_string()).unwrap();

        let error = lexer.generate_tokens().unwrap_err();

        assert_eq!(error, LexerError::ExhaustedInput)
    }

    #[test]
    fn fail_to_parse_unclosed_strings_2_with_single_quote() {
        let query: &str = "ABC'";

        let lexer = Lexer::new(query.to_string()).unwrap();

        let error = lexer.generate_tokens().unwrap_err();

        assert_eq!(error, LexerError::InvalidIdentifierCharacter('\''))
    }

    #[test]
    fn parse_string() {
        let query: &str = "\"marduk\"";
        let expected_tokens = vec![Token::String("marduk".to_string())];

        let lexer = Lexer::new(query.to_string()).unwrap();

        match lexer.generate_tokens() {
            Ok(tokens) => assert_eq!(tokens, expected_tokens),
            Err(err) => panic!("Test failed with error: {:?}", err),
        }
    }

    #[test]
    fn parse_string_with_single_quote() {
        let query: &str = "'marduk'";
        let expected_tokens = vec![Token::String("marduk".to_string())];

        let lexer = Lexer::new(query.to_string()).unwrap();

        match lexer.generate_tokens() {
            Ok(tokens) => assert_eq!(tokens, expected_tokens),
            Err(err) => panic!("Test failed with error: {:?}", err),
        }
    }

    #[test]
    fn fail_to_parse_invalid_integers() {
        let query: &str = "1a0";

        let lexer = Lexer::new(query.to_string()).unwrap();

        let error = lexer.generate_tokens().unwrap_err();

        assert_eq!(error, LexerError::InvalidInteger(1))
    }

    #[test]
    fn fail_to_parse_single_minus_character() {
        let query: &str = "-";

        let lexer = Lexer::new(query.to_string()).unwrap();

        let error = lexer.generate_tokens().unwrap_err();

        assert_eq!(error, LexerError::InvalidInteger(0))
    }

    #[test]
    fn parse_positive_integer() {
        let query: &str = "10";
        let expected_tokens = vec![Token::Integer(10)];

        let lexer = Lexer::new(query.to_string()).unwrap();

        match lexer.generate_tokens() {
            Ok(tokens) => assert_eq!(tokens, expected_tokens),
            Err(err) => panic!("Test failed with error: {:?}", err),
        }
    }

    #[test]
    fn parse_negative_integer() {
        let query: &str = "-10";
        let expected_tokens = vec![Token::Integer(-10)];

        let lexer = Lexer::new(query.to_string()).unwrap();

        match lexer.generate_tokens() {
            Ok(tokens) => assert_eq!(tokens, expected_tokens),
            Err(err) => panic!("Test failed with error: {:?}", err),
        }
    }

    #[test]
    fn parse_get_query_with_object_name() {
        let query: &str = "get process_az(name, start_time)";

        let lexer = Lexer::new(query.to_string()).unwrap();

        let expected_tokens = vec![
            Token::Keyword(Keyword::Get),
            Token::Word("process_az".to_string()),
            Token::Operator(Operator::OpenParantheses),
            Token::Word("name".to_string()),
            Token::Operator(Operator::Comma),
            Token::Word("start_time".to_string()),
            Token::Operator(Operator::CloseParantheses),
        ];

        match lexer.generate_tokens() {
            Ok(tokens) => assert_eq!(tokens, expected_tokens),
            Err(err) => panic!("Test failed with error: {:?}", err),
        }
    }

    #[test]
    fn parse_get_query_with_pick_keyword() {
        let query: &str = "get process pick name = \"marduk\" AND pid > 100 AND temp = -12";
        let lexer = Lexer::new(query.to_string()).unwrap();

        let expected_tokens = vec![
            Token::Keyword(Keyword::Get),
            Token::Word("process".to_string()),
            Token::Keyword(Keyword::Pick),
            Token::Word("name".to_string()),
            Token::Operator(Operator::Equals),
            Token::String("marduk".to_string()),
            Token::Keyword(Keyword::And),
            Token::Word("pid".to_string()),
            Token::Operator(Operator::BiggerThan),
            Token::Integer(100),
            Token::Keyword(Keyword::And),
            Token::Word("temp".to_string()),
            Token::Operator(Operator::Equals),
            Token::Integer(-12),
        ];

        match lexer.generate_tokens() {
            Ok(tokens) => assert_eq!(tokens, expected_tokens),
            Err(err) => panic!("Test failed with error: {:?}", err),
        }
    }

    #[test]
    fn parse_order_keyword() {
        let query: &str = "order name ASC";
        let lexer = Lexer::new(query.to_string()).unwrap();

        let expected_tokens = vec![
            Token::Keyword(Keyword::Order),
            Token::Word("name".to_string()),
            Token::Keyword(Keyword::Asc),
        ];

        match lexer.generate_tokens() {
            Ok(tokens) => assert_eq!(tokens, expected_tokens),
            Err(err) => panic!("Test failed with error: {:?}", err),
        }
    }

    #[test]
    fn parse_format_keyword() {
        let query: &str = "format YAML";
        let lexer = Lexer::new(query.to_string()).unwrap();

        let expected_tokens = vec![
            Token::Keyword(Keyword::Format),
            Token::Word("YAML".to_string()),
        ];

        match lexer.generate_tokens() {
            Ok(tokens) => assert_eq!(tokens, expected_tokens),
            Err(err) => panic!("Test failed with error: {:?}", err),
        }
    }
}
