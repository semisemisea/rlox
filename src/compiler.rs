use crate::{
    comp::{
        op_code::{Chunk, OpCode},
        parsr_rules::{DEFAULT_RULE, ParseResult, ParseRule, Precedence, rule_map},
    },
    gc,
    lox_object::{
        lox_function::{FuncType, LoxFunction},
        lox_string::LoxString,
    },
    object::{LoxObj, LoxObjType},
    scanner::{Scanner, Token, TokenError, TokenType},
    value::Value,
};
use bytes::Bytes;

#[derive(Debug)]
pub struct Parser<'a> {
    prev: Token,
    curr: Token,
    scanner: Scanner<'a>,
    compiler: *mut Compiler,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>, compiler: *mut Compiler) -> Parser<'a> {
        Parser {
            prev: Token::placeholder(),
            curr: Token::placeholder(),
            scanner,
            compiler,
        }
    }

    fn chunk(&mut self) -> &mut Chunk {
        unsafe { &mut (*(*self.compiler).function).chunk }
    }

    // NOTE: Core method to write a byte into a VM bytecode Chunk.
    fn emit_byte<T: Into<u8>>(&mut self, byte: T) {
        let line_no = self.prev.line;
        self.chunk().write_in(byte.into(), line_no)
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Nil);
        self.emit_byte(OpCode::Return);
    }

    // NOTE: Number Literal branch.
    // Also load a constant number into constants pool;
    pub fn number(&mut self, _can_assign: bool) -> ParseResult {
        let num = self.scanner.make_str(self.prev).parse::<f64>().unwrap();
        let val = Value::new_num(num);
        self.emit_constant(val)
    }

    // NOTE: Load a constant. Also store it in the pool.
    fn emit_constant(&mut self, val: Value) -> ParseResult {
        self.emit_byte(OpCode::Constant);
        let idx = self.chunk().add_constant(val);
        if idx > u8::MAX as usize {
            return Err(CompilationError::TooMuchConstants);
        }
        self.emit_byte(idx as u8);
        Ok(())
    }

    fn emit_jump(&mut self, instruction: OpCode) -> usize {
        self.emit_byte(instruction);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.chunk().ip_len() - 2
    }

    // WARNING: Need to handle result here. But just unwrap for now.
    fn advance(&mut self) -> Result<(), TokenError> {
        self.prev = self.curr;
        self.curr = self.scanner.scan_token()?;
        Ok(())
    }

    pub fn literal(&mut self, _can_assign: bool) -> ParseResult {
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
            Err(CompilationError::NotWantedToken {
                desire_type: token_type,
                actual: self.curr,
            })
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.curr.token_type == token_type
    }

    // NOTE: ('inside') Parenthesis branch.
    pub fn grouping(&mut self, _can_assign: bool) -> ParseResult {
        self.expression()?;
        self.match_and_consume(TokenType::RParen)
    }

    // NOTE: Unary branch.
    pub fn unary(&mut self, _can_assign: bool) -> ParseResult {
        let token_type = self.prev.token_type;
        self.parse(Precedence::Unary)?;
        match token_type {
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            TokenType::Negation => self.emit_byte(OpCode::Not),
            _ => unreachable!(),
        }
        Ok(())
    }

    pub fn binary(&mut self, _can_assign: bool) -> ParseResult {
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

    pub fn string(&mut self, _can_assign: bool) -> ParseResult {
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
            return Err(CompilationError::NoPrefixRule(self.prev));
        };
        let can_assign = prece <= Precedence::Assignment;
        prefix_rule(self, can_assign)?;
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
        } else if self.match_and_consume(TokenType::Fun).is_ok() {
            return self.fun_declaration();
        };
        self.statement()
    }

    fn statement(&mut self) -> ParseResult {
        if self.match_and_consume(TokenType::Print).is_ok() {
            self.print_statement()
        } else if self.match_and_consume(TokenType::LBrace).is_ok() {
            self.new_scope();
            self.block()?;
            self.del_scope();
            Ok(())
        } else if self.match_and_consume(TokenType::If).is_ok() {
            self.if_statement()
        } else if self.match_and_consume(TokenType::While).is_ok() {
            self.while_statement()
        } else if self.match_and_consume(TokenType::Return).is_ok() {
            self.return_statement()
        } else {
            self.expression_statement()
        }
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
        unsafe {
            self.match_and_consume(TokenType::Identifier)?;
            self.declare_variable()?;
            if (*self.compiler).scope_depth > 0 {
                return Ok(0);
            }
            let ident = self.scanner.make_str(self.prev).to_owned();
            let val = Value::new_string(ident);
            let idx = self.chunk().add_constant(val);
            Ok(idx)
        }
    }

    fn mark_initialized(&mut self) -> bool {
        unsafe {
            if (*self.compiler).scope_depth > 0 {
                (*self.compiler).local[(*self.compiler).local_cnt - 1].depth =
                    (*self.compiler).scope_depth;
                true
            } else {
                false
            }
        }
    }

    fn define_variable(&mut self, glob_var_idx: usize) {
        if self.mark_initialized() {
            return;
        }
        self.emit_byte(OpCode::DefGlob);
        self.emit_byte(glob_var_idx as u8);
    }

    pub fn variable(&mut self, can_assign: bool) -> ParseResult {
        self.named_variable(can_assign)
    }

    fn declare_variable(&mut self) -> ParseResult {
        unsafe {
            if (*self.compiler).scope_depth == 0 {
                return Ok(());
            }
            let name = self.prev;
            for local in self.compiler.as_ref().unwrap().local[..(*self.compiler).local_cnt]
                .iter()
                .rev()
                .take_while(|local| {
                    !(local.depth != usize::MAX && local.depth < (*self.compiler).scope_depth)
                })
            {
                if self.scanner.make_str(name) == self.scanner.make_str(local.name) {
                    return Err(CompilationError::LocalVariableRedefine);
                }
            }
            self.add_local(name)?;
            // for i in 0..(*self.compiler).local_cnt {
            //     eprintln!("{:?}", (*self.compiler).local[i]);
            // }
            Ok(())
        }
    }

    fn add_local(&mut self, name: Token) -> ParseResult {
        unsafe {
            if (*self.compiler).local_cnt == (*self.compiler).local.len() {
                return Err(CompilationError::TooMuchStackVariable);
            }
            let local = Local {
                name,
                depth: usize::MAX,
            };
            (*self.compiler).local[(*self.compiler).local_cnt] = local;
            (*self.compiler).local_cnt += 1;
            Ok(())
        }
    }

    fn named_variable(&mut self, can_assign: bool) -> ParseResult {
        let ident = self.scanner.make_str(self.prev).to_owned();
        let val = Value::new_string(ident);
        let (get_op, set_op, idx) = match self.resolve_local(self.prev)? {
            Some(idx) => (OpCode::GetLocal, OpCode::SetLocal, idx),
            None => (
                OpCode::GetGlob,
                OpCode::SetGlob,
                self.chunk().add_constant(val),
            ),
        };
        if can_assign && self.match_and_consume(TokenType::Assign).is_ok() {
            self.expression()?;
            self.emit_byte(set_op);
        } else {
            self.emit_byte(get_op);
        }
        self.emit_byte(idx as u8);
        Ok(())
    }

    fn block(&mut self) -> ParseResult {
        while !(self.check(TokenType::RBrace) || self.check(TokenType::Eof)) {
            self.declaration()?;
        }
        self.match_and_consume(TokenType::RBrace)
    }

    fn new_scope(&mut self) {
        unsafe {
            (*self.compiler).scope_depth += 1;
        }
    }

    fn del_scope(&mut self) {
        unsafe {
            (*self.compiler).scope_depth -= 1;
            while (*self.compiler).local_cnt > 0
                && (*self.compiler).local[(*self.compiler).local_cnt - 1].depth
                    > (*self.compiler).scope_depth
            {
                self.emit_byte(OpCode::Pop);
                (*self.compiler).local_cnt -= 1;
            }
        }
    }

    fn resolve_local(&self, name: Token) -> Result<Option<usize>, CompilationError> {
        for (idx, local) in unsafe {
            self.compiler.as_ref().unwrap().local[..(*self.compiler).local_cnt]
                .iter()
                .enumerate()
                .rev()
        } {
            if self.scanner.make_str(local.name) == self.scanner.make_str(name) {
                if local.depth == usize::MAX {
                    return Err(CompilationError::ReadWhileDefining);
                }
                return Ok(Some(idx));
            }
        }
        Ok(None)
    }

    fn if_statement(&mut self) -> ParseResult {
        self.match_and_consume(TokenType::LParen)?;
        self.expression()?;
        self.match_and_consume(TokenType::RParen)?;

        let then_offset = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop);
        self.statement()?;
        let else_offset = self.emit_jump(OpCode::Jump);
        self.patch_jump(then_offset)?;
        self.emit_byte(OpCode::Pop);

        if self.match_and_consume(TokenType::Else).is_ok() {
            self.statement()?;
        }
        self.patch_jump(else_offset)?;
        Ok(())
    }

    fn patch_jump(&mut self, offset: usize) -> ParseResult {
        let jump_to = self.chunk().ip_len() - offset - 2;
        if jump_to > u16::MAX as usize {
            return Err(CompilationError::ExceedJumpLimit);
        }
        self.chunk().modify(offset, (jump_to >> 8) as u8);
        self.chunk().modify(offset + 1, jump_to as u8);
        Ok(())
    }

    pub fn and(&mut self, _can_assign: bool) -> ParseResult {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_jump(OpCode::Pop);
        self.parse(Precedence::And)?;
        self.patch_jump(end_jump)
    }

    pub fn or(&mut self, _can_assign: bool) -> ParseResult {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(else_jump)?;
        self.emit_byte(OpCode::Pop);
        self.parse(Precedence::Or)?;
        self.patch_jump(end_jump)?;
        self.patch_jump(end_jump)
    }

    fn while_statement(&mut self) -> ParseResult {
        let loop_start = self.chunk().ip_len();
        self.match_and_consume(TokenType::LParen)?;
        self.expression()?;
        self.match_and_consume(TokenType::RParen)?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop);
        self.statement()?;
        self.emit_loop(loop_start)?;
        self.patch_jump(exit_jump)?;
        self.emit_byte(OpCode::Pop);
        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) -> ParseResult {
        self.emit_byte(OpCode::Loop);
        let offset = self.chunk().ip_len() - loop_start + 2;
        if offset > u16::MAX as usize {
            return Err(CompilationError::ExceedJumpLimit);
        }
        self.emit_byte((offset >> 8) as u8);
        self.emit_byte(offset as u8);
        Ok(())
    }

    fn fun_declaration(&mut self) -> ParseResult {
        let glob_func_idx = self.parse_variable()?;
        self.mark_initialized();
        self.function(FuncType::Function)?;
        self.define_variable(glob_func_idx);
        Ok(())
    }

    fn function(&mut self, func_type: FuncType) -> ParseResult {
        Compiler::update(
            &mut self.compiler,
            LoxString::new(self.scanner.make_str(self.prev).to_string()),
            func_type,
        );
        self.new_scope();
        self.match_and_consume(TokenType::LParen)?;
        if !self.check(TokenType::RParen) {
            loop {
                unsafe {
                    if (*(*self.compiler).function).arity == 255 {
                        return Err(CompilationError::ExceedArityLimit);
                    }
                    (*(*self.compiler).function).arity += 1;
                    let const_idx = self.parse_variable()?;
                    self.define_variable(const_idx);
                    if self.match_and_consume(TokenType::Comma).is_err() {
                        break;
                    }
                }
            }
        }
        self.match_and_consume(TokenType::RParen)?;
        self.match_and_consume(TokenType::LBrace)?;
        self.block()?;
        self.emit_return();
        let obj_function = Compiler::end_compiler(&mut self.compiler);
        let val = Value::new_function(obj_function);
        self.emit_constant(val)
    }

    pub fn call(&mut self, _can_assign: bool) -> ParseResult {
        let argc = self.argument_list()?;
        self.emit_byte(OpCode::Call);
        self.emit_byte(argc);
        Ok(())
    }

    fn argument_list(&mut self) -> Result<u8, CompilationError> {
        let mut arg_count = 0;
        if !self.check(TokenType::RParen) {
            loop {
                self.expression()?;
                if arg_count == 255 {
                    return Err(CompilationError::TooMuchArgument);
                }
                arg_count += 1;
                if self.match_and_consume(TokenType::Comma).is_err() {
                    break;
                }
            }
        }
        self.match_and_consume(TokenType::RParen)?;
        Ok(arg_count)
    }

    fn return_statement(&mut self) -> ParseResult {
        if matches!(unsafe { &(*self.compiler).func_type }, FuncType::Script) {
            return Err(CompilationError::ReturnInTopFunction);
        }
        if self.match_and_consume(TokenType::Semicolon).is_ok() {
            self.emit_return();
        } else {
            self.expression()?;
            self.match_and_consume(TokenType::Semicolon)?;
            self.emit_byte(OpCode::Return);
        }
        Ok(())
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct Local {
    name: Token,
    depth: usize,
}

const LOCAL_SIZE: usize = u8::MAX as usize + 1;

#[derive(Debug)]
pub struct Compiler {
    function: *mut LoxFunction,
    func_type: FuncType,
    local: [Local; LOCAL_SIZE],
    local_cnt: usize,
    scope_depth: usize,
    enclosing: *mut Compiler,
}

impl Compiler {
    /// param 'name' must be registered.
    fn new(name: *const LoxString, func_type: FuncType) -> *mut Compiler {
        let obj = LoxObj {
            obj_type: LoxObjType::Function,
            next: std::ptr::null_mut(),
        };
        let func = LoxFunction {
            name,
            obj,
            arity: 0,
            chunk: Chunk::new(),
        };
        let func_ptr = Box::into_raw(Box::new(func));

        gc::register(func_ptr);
        Box::into_raw(Box::new(Compiler {
            function: func_ptr,
            func_type,
            local: [Local::default(); LOCAL_SIZE],
            local_cnt: 1,
            scope_depth: 0,
            enclosing: std::ptr::null_mut(),
        }))
    }

    fn update(ptr: &mut *mut Compiler, name: *const LoxString, func_type: FuncType) {
        let obj = LoxObj {
            obj_type: LoxObjType::Function,
            next: std::ptr::null_mut(),
        };
        let func = LoxFunction {
            name,
            obj,
            arity: 0,
            chunk: Chunk::new(),
        };
        let func_ptr = Box::into_raw(Box::new(func));

        gc::register(func_ptr);
        *ptr = Box::into_raw(Box::new(Compiler {
            function: func_ptr,
            func_type,
            local: [Local::default(); LOCAL_SIZE],
            local_cnt: 1,
            scope_depth: 0,
            enclosing: *ptr,
        }))
    }

    fn end_compiler(ptr: &mut *mut Compiler) -> *mut LoxFunction {
        unsafe {
            let ret = (**ptr).function;
            let prev = (**ptr).enclosing;
            let _ = Box::from_raw(*ptr);
            *ptr = prev;
            ret
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CompilationError {
    #[error("Too much constants...")]
    TooMuchConstants,
    #[error("{actual:?} is not the wanted token {desire_type:?}...")]
    NotWantedToken {
        desire_type: TokenType,
        actual: Token,
    },
    #[error("This operator {0:?} got no prefix rule...")]
    NoPrefixRule(Token),
    #[error("This is not a valid assignment...")]
    InvalidAssignment,
    #[error("You've create too much stack variable...")]
    TooMuchStackVariable,
    #[error("You redefine a local variable in the same scope twice...")]
    LocalVariableRedefine,
    #[error("You are trying to read a variable while defining it...")]
    ReadWhileDefining,
    #[error("Too much code to jump over...")]
    ExceedJumpLimit,
    #[error("Can't have more than 255 arguments...")]
    TooMuchArgument,
    #[error("You can't return from a top level function...")]
    ReturnInTopFunction,
    #[error("Can't have more than 255 parameters.")]
    ExceedArityLimit,
}

#[derive(Debug, thiserror::Error)]
pub enum CompileTimeError {
    #[error("Stage: Tokenization; Error msg: {0:?}")]
    Tokenization(#[from] TokenError),
    #[error("Stage: Compilation; Error msg: {0:?}")]
    Compilation(#[from] CompilationError),
}

pub fn compile(source: &Bytes) -> Result<*const LoxFunction, CompileTimeError> {
    let scanner = Scanner::new(source);
    let compiler = Compiler::new(LoxString::new(String::from("main")), FuncType::Script);
    let mut parser = Parser::new(scanner, compiler);

    parser.advance().map_err(CompileTimeError::Tokenization)?;
    while parser.match_and_consume(TokenType::Eof).is_err() {
        parser
            .declaration()
            .map_err(CompileTimeError::Compilation)?;
    }
    parser.emit_return();
    let main_func_ptr = Compiler::end_compiler(&mut parser.compiler);

    Ok(main_func_ptr)
}
