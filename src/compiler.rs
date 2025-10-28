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
        map.insert(
            TokenType::StringLiteral,
            ParseRule::new(Some(string), None, Precedence::None),
        );
        map.insert(
            TokenType::Identifier,
            ParseRule::new(Some(variable), None, Precedence::None),
        );

        map
    })
}

fn number(parser: &mut Parser, can_assign: bool) -> ParseResult {
    parser.number(can_assign)
}

fn binary(parser: &mut Parser, can_assign: bool) -> ParseResult {
    parser.binary(can_assign)
}

fn unary(parser: &mut Parser, can_assign: bool) -> ParseResult {
    parser.unary(can_assign)
}

fn grouping(parser: &mut Parser, can_assign: bool) -> ParseResult {
    parser.grouping(can_assign)
}

fn literal(parser: &mut Parser, can_assign: bool) -> ParseResult {
    parser.literal(can_assign)
}

fn string(parser: &mut Parser, can_assign: bool) -> ParseResult {
    parser.string(can_assign)
}

fn variable(parser: &mut Parser, can_assign: bool) -> ParseResult {
    parser.variable(can_assign)
}

#[derive(Debug)]
pub struct Parser<'a> {
    prev: Token,
    curr: Token,
    chunk: &'a mut Chunk,
    scanner: Scanner<'a>,
}

type ParseResult = Result<(), CompilationError>;
type ParseFn = for<'a, 'b> fn(&'b mut Parser<'a>, bool) -> ParseResult;

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
    fn number(&mut self, can_assign: bool) -> ParseResult {
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

    fn literal(&mut self, can_assign: bool) -> ParseResult {
        match self.prev.token_type {
            TokenType::False => self.emit_byte(OpCode::False),
            TokenType::True => self.emit_byte(OpCode::True),
            TokenType::Nil => self.emit_byte(OpCode::Nil),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn match_and_consume(&mut self, token_type: TokenType) -> ParseResult {
        if token_type == self.curr.token_type {
            // TODO: Error handling.
            self.advance().unwrap();
            Ok(())
        } else {
            Err(CompilationError::NotWantedToken)
        }
    }

    // NOTE: ('inside') Parenthesis branch.
    fn grouping(&mut self, can_assign: bool) -> ParseResult {
        self.expression()?;
        self.match_and_consume(TokenType::RParen)
    }

    // NOTE: Unary branch.
    fn unary(&mut self, can_assign: bool) -> ParseResult {
        let token_type = self.prev.token_type;
        self.parse(Precedence::Unary)?;
        match token_type {
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            TokenType::Negation => self.emit_byte(OpCode::Not),
            _ => unreachable!(),
        }
        Ok(())
    }

    fn binary(&mut self, can_assign: bool) -> ParseResult {
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

    fn string(&mut self, can_assign: bool) -> ParseResult {
        let str = self.scanner.make_str(self.prev).to_owned();
        self.emit_constant(Value::new_string(str))?;
        Ok(())
    }

    #[inline(always)]
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
            return Err(CompilationError::NoPrefixRule);
        };
        let can_assign = prece <= Precedence::Assignment;
        prefix_rule(self, can_assign).unwrap();
        while prece <= self.get_rule_curr().precedence {
            // TODO: Error handling.
            self.advance().unwrap();
            let Some(infix_rule) = self.get_rule_prev().infix else {
                unreachable!()
            };
            infix_rule(self, can_assign)?;
        }
        if can_assign && self.match_and_consume(TokenType::Assign).is_ok() {
            return Err(CompilationError::InvalidAssignment);
        }
        Ok(())
    }

    fn declaration(&mut self) -> ParseResult {
        if self.match_and_consume(TokenType::Var).is_ok() {
            return self.var_declaration();
        };
        self.statement()
    }

    fn statement(&mut self) -> ParseResult {
        if self.match_and_consume(TokenType::Print).is_ok() {
            self.print_statement()
        } else {
            self.expression_statement()
        }
        // match self.curr.token_type {
        //     TokenType::Print => self.print_statement(),
        //     _ => todo!("{:?}", self.curr),
        // }
    }

    fn print_statement(&mut self) -> ParseResult {
        self.expression()?;
        self.match_and_consume(TokenType::Semicolon)?;
        self.emit_byte(OpCode::Print);
        Ok(())
    }

    fn expression_statement(&mut self) -> ParseResult {
        self.expression()?;
        self.match_and_consume(TokenType::Semicolon)?;
        self.emit_byte(OpCode::Pop);
        Ok(())
    }

    fn var_declaration(&mut self) -> ParseResult {
        let glob_var_idx = self.parse_variable()?;
        if self.match_and_consume(TokenType::Assign).is_ok() {
            self.expression()?;
        } else {
            self.emit_byte(OpCode::Nil);
        }
        self.match_and_consume(TokenType::Semicolon)?;
        self.define_variable(glob_var_idx);
        Ok(())
    }

    fn parse_variable(&mut self) -> Result<usize, CompilationError> {
        self.match_and_consume(TokenType::Identifier)?;
        let ident = self.scanner.make_str(self.prev).to_owned();
        let val = Value::new_string(ident);
        let idx = self.chunk.add_constant(val);
        Ok(idx)
    }

    fn define_variable(&mut self, glob_var_idx: usize) {
        self.emit_byte(OpCode::DefGlob);
        self.emit_byte(glob_var_idx as u8);
    }

    fn variable(&mut self, can_assign: bool) -> ParseResult {
        self.named_variable(can_assign)
    }

    fn named_variable(&mut self, can_assign: bool) -> ParseResult {
        let ident = self.scanner.make_str(self.prev).to_owned();
        let val = Value::new_string(ident);
        let idx = self.chunk.add_constant(val);
        if can_assign && self.match_and_consume(TokenType::Assign).is_ok() {
            self.expression()?;
            self.emit_byte(OpCode::DefGlob);
        } else {
            self.emit_byte(OpCode::GetGlob);
        }
        self.emit_byte(idx as u8);
        Ok(())
    }
}

#[derive(Debug)]
struct Local {
    name: Token,
    depth: usize,
}

const LOCAL_SIZE: usize = u8::MAX as usize + 1;

#[derive(Debug)]
pub struct Compiler {
    local: [Local; LOCAL_SIZE],
    local_cnt: usize,
    scope_depth: usize,
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
    NoPrefixRule,
    InvalidAssignment,
}

#[derive(Debug)]
pub enum CompileTimeError {
    Tokenization(TokenError),
    Compilation(CompilationError),
}

pub fn compile(source: &Bytes) -> Result<Chunk, CompileTimeError> {
    let mut chunk = Chunk::new();
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(&mut chunk, scanner);

    parser.advance().map_err(CompileTimeError::Tokenization)?;
    while parser.match_and_consume(TokenType::Eof).is_err() {
        parser
            .declaration()
            .map_err(CompileTimeError::Compilation)?;
    }

    chunk.write_in(OpCode::Return as _, 1);

    Ok(chunk)
}
