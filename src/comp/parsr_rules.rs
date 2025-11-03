use std::{collections::HashMap, sync::OnceLock};

use crate::{
    compiler::{CompilationError, Parser},
    scanner::TokenType,
};

pub type ParseResult = Result<(), CompilationError>;
pub type ParseFn = for<'a, 'b> fn(&'b mut Parser<'a>, bool) -> ParseResult;

pub static DEFAULT_RULE: ParseRule = ParseRule {
    prefix: None,
    infix: None,
    precedence: Precedence::None,
};

pub fn rule_map() -> &'static HashMap<TokenType, ParseRule> {
    static MAP: OnceLock<HashMap<TokenType, ParseRule>> = OnceLock::new();
    MAP.get_or_init(|| {
        let mut map = HashMap::new();
        map.insert(
            TokenType::LParen,
            ParseRule::new(Some(grouping), Some(call), Precedence::Call),
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
        map.insert(
            TokenType::And,
            ParseRule::new(None, Some(and), Precedence::And),
        );
        map.insert(
            TokenType::Or,
            ParseRule::new(None, Some(or), Precedence::Or),
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

fn and(parser: &mut Parser, can_assign: bool) -> ParseResult {
    parser.and(can_assign)
}

fn or(parser: &mut Parser, can_assign: bool) -> ParseResult {
    parser.or(can_assign)
}

fn call(parser: &mut Parser, can_assign: bool) -> ParseResult {
    parser.call(can_assign)
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
    pub fn next_prece(&self) -> Precedence {
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
pub struct ParseRule {
    pub prefix: Option<ParseFn>,
    pub infix: Option<ParseFn>,
    pub precedence: Precedence,
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
