use crate::{
    comp::op_code::{Chunk, OpCode},
    scanner::{Scanner, Token, TokenError, TokenType},
    value::Value,
};
use bytes::Bytes;
use std::{collections::HashMap, sync::OnceLock};

static DEFAULT_RULE: ParseRule = ParseRule {
    prefix: None,
    infix: None,
    precedence: Precedence::None,
};

fn rule_map() -> &'static HashMap<TokenType, ParseRule> {
    static MAP: OnceLock<HashMap<TokenType, ParseRule>> = OnceLock::new();
    MAP.get_or_init(|| {
        let mut map = HashMap::new();
        map.insert(
            TokenType::LParen,
            ParseRule::new(Some(grouping), None, Precedence::None),
        );
        map.insert(
            TokenType::Minus,
            ParseRule::new(Some(unary), Some(binary), Precedence::Term),
        );
        map.insert(
            TokenType::Plus,
            ParseRule::new(None, Some(binary), Precedence::Term),
        );
        map.insert(
            TokenType::Slash,
            ParseRule::new(None, Some(binary), Precedence::Factor),
        );
        map.insert(
            TokenType::Star,
            ParseRule::new(None, Some(binary), Precedence::Factor),
        );
        map.insert(
            TokenType::NumberLiteral,
            ParseRule::new(Some(number), None, Precedence::None),
        );
        map.insert(
            TokenType::False,
            ParseRule::new(Some(literal), None, Precedence::None),
        );
        map.insert(
            TokenType::True,
            ParseRule::new(Some(literal), None, Precedence::None),
        );
        map.insert(
            TokenType::Nil,
            ParseRule::new(Some(literal), None, Precedence::None),
        );
        map.insert(
            TokenType::Negation,
            ParseRule::new(Some(unary), None, Precedence::None),
        );
        map.insert(
            TokenType::Equal,
            ParseRule::new(None, Some(binary), Precedence::Equality),
        );
        map.insert(
            TokenType::Inequal,
            ParseRule::new(None, Some(binary), Precedence::Equality),
        );
        map.insert(
            TokenType::GreaterThan,
            ParseRule::new(None, Some(binary), Precedence::Comparison),
        );
        map.insert(
            TokenType::GreaterThanOrEqual,
            ParseRule::new(None, Some(binary), Precedence::Comparison),
        );
        map.insert(
            TokenType::LessThan,
            ParseRule::new(None, Some(binary), Precedence::Comparison),
        );
        map.insert(
            TokenType::LessThanOrEqual,
            ParseRule::new(None, Some(binary), Precedence::Comparison),
        );

        map
    })
}

fn number(parser: &mut Parser) -> ParseResult {
    parser.number()
}

fn binary(parser: &mut Parser) -> ParseResult {
    parser.binary()
}

fn unary(parser: &mut Parser) -> ParseResult {
    parser.unary()
}

fn grouping(parser: &mut Parser) -> ParseResult {
    parser.grouping()
}

fn literal(parser: &mut Parser) -> ParseResult {
    parser.literal()
}

#[derive(Debug)]
pub struct Parser<'a> {
    prev: Token,
    curr: Token,
    chunk: &'a mut Chunk,
    scanner: Scanner<'a>,
}

type ParseResult = Result<(), CompilationError>;
type ParseFn = for<'a, 'b> fn(&'b mut Parser<'a>) -> ParseResult;

impl<'a> Parser<'a> {
    pub fn new(chunk: &'a mut Chunk, scanner: Scanner<'a>) -> Parser<'a> {
        Parser {
            prev: Token::placeholder(),
            curr: Token::placeholder(),
            chunk,
            scanner,
        }
    }

    // NOTE: Core method to write a byte into a VM bytecode Chunk.
    fn emit_byte<T: Into<u8>>(&mut self, byte: T) {
        self.chunk
            // TODO: May be self.curr -> self.prev       here.
            .write_in(byte.into(), self.prev.line);
    }

    // NOTE: Number Literal branch.
    // Also load a constant number into constants pool;
    fn number(&mut self) -> ParseResult {
        let num = self.scanner.make_str(self.prev).parse::<f64>().unwrap();
        let val = Value::new_num(num);
        self.emit_constant(val)
    }

    // NOTE: Load a constant. Also store it in the pool.
    fn emit_constant(&mut self, val: Value) -> ParseResult {
        self.emit_byte(OpCode::Constant);
        let idx = self.chunk.add_constant(val);
        if idx > u8::MAX as usize {
            return Err(CompilationError::TooMuchConstants);
        }
        self.emit_byte(idx as u8);
        Ok(())
    }

    // NOTE: Add a return.
    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return);
    }

    // WARNING: Need to handle result here. But just unwrap for now.
    fn advance(&mut self) -> Result<(), TokenError> {
        self.prev = self.curr;
        self.curr = self.scanner.scan_token()?;
        Ok(())
    }

    fn literal(&mut self) -> ParseResult {
        match self.prev.token_type {
            TokenType::False => self.emit_byte(OpCode::False),
            TokenType::True => self.emit_byte(OpCode::True),
            TokenType::Nil => self.emit_byte(OpCode::Nil),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn consume(&mut self, token_type: TokenType) -> ParseResult {
        if token_type == self.curr.token_type {
            // TODO: Error handling.
            self.advance().unwrap();
            Ok(())
        } else {
            Err(CompilationError::NotWantedToken)
        }
    }

    // NOTE: ('inside') Parenthesis branch.
    fn grouping(&mut self) -> ParseResult {
        self.expression()?;
        self.consume(TokenType::RParen)
    }

    // NOTE: Unary branch.
    fn unary(&mut self) -> ParseResult {
        let token_type = self.prev.token_type;
        self.parse(Precedence::Unary)?;
        match token_type {
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            TokenType::Negation => self.emit_byte(OpCode::Not),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn binary(&mut self) -> ParseResult {
        let prev_type = self.prev.token_type;
        let rule = self.get_rule_prev();
        self.parse(rule.precedence.next_prece())?;

        match prev_type {
            TokenType::Plus => self.emit_byte(OpCode::Add),
            TokenType::Minus => self.emit_byte(OpCode::Subtract),
            TokenType::Star => self.emit_byte(OpCode::Multiply),
            TokenType::Slash => self.emit_byte(OpCode::Divide),
            TokenType::Equal => self.emit_byte(OpCode::Equal),
            TokenType::GreaterThan => self.emit_byte(OpCode::Greater),
            TokenType::LessThan => self.emit_byte(OpCode::Less),
            TokenType::GreaterThanOrEqual => {
                self.emit_byte(OpCode::Less);
                self.emit_byte(OpCode::Not);
            }
            TokenType::LessThanOrEqual => {
                self.emit_byte(OpCode::Greater);
                self.emit_byte(OpCode::Not);
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn expression(&mut self) -> ParseResult {
        self.parse(Precedence::Assignment)?;

        Ok(())
    }

    #[inline(always)]
    fn get_rule_prev(&self) -> &ParseRule {
        rule_map()
            .get(&self.prev.token_type)
            .unwrap_or(&DEFAULT_RULE)
    }

    #[inline(always)]
    fn get_rule_curr(&self) -> &ParseRule {
        rule_map()
            .get(&self.curr.token_type)
            .unwrap_or(&DEFAULT_RULE)
    }

    pub fn parse(&mut self, prece: Precedence) -> ParseResult {
        // TODO: Error handling.
        self.advance().unwrap();
        let rule = self.get_rule_prev();

        let Some(prefix_rule) = rule.prefix else {
            return Err(CompilationError::NoPrefixPlaceHolder1);
        };
        prefix_rule(self).unwrap();
        while prece <= self.get_rule_curr().precedence {
            // TODO: Error handling.
            self.advance().unwrap();
            let Some(infix_rule) = self.get_rule_prev().infix else {
                unreachable!()
            };
            infix_rule(self)?;
        }
        Ok(())
    }
}

#[repr(C)]
#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub enum Precedence {
    #[default]
    None = 0,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // ==  !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn next_prece(&self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

#[derive(Default, Debug, Clone)]
struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(prefix: Option<ParseFn>, infix: Option<ParseFn>, precedence: Precedence) -> ParseRule {
        ParseRule {
            prefix,
            infix,
            precedence,
        }
    }
}

#[derive(Debug)]
pub enum CompilationError {
    TooMuchConstants,
    UnresolvedRParen,
    NotWantedToken,
    NoPrefixPlaceHolder1,
}

#[derive(Debug)]
pub enum CompileTimeError {
    Tokenization(TokenError),
    Compilation(CompilationError),
}

// BUG: Wait to fix.
pub fn compile(source: &Bytes) -> Result<Chunk, CompileTimeError> {
    let mut chunk = Chunk::new();
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(&mut chunk, scanner);

    parser.advance().map_err(CompileTimeError::Tokenization)?;
    parser.expression().map_err(CompileTimeError::Compilation)?;
    parser
        .consume(TokenType::Eof)
        .map_err(CompileTimeError::Compilation)?;
    chunk.write_in(OpCode::Return as _, 1);

    Ok(chunk)
}
