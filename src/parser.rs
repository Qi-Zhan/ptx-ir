use crate::ir::*;
use crate::lexer::Token;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use logos::{Logos as _, Span};

pub struct Parser<'a> {
    file_id: usize,
    tokens: std::iter::Peekable<logos::SpannedIter<'a, Token>>,
}

type ParseResult<T> = Result<T, Diagnostic<usize>>;

impl<'a> Parser<'a> {
    pub fn new(file_id: usize, source: &'a str) -> Self {
        Self {
            file_id,
            tokens: Token::lexer(source).spanned().peekable(),
        }
    }

    pub fn parse(&mut self) -> ParseResult<Module> {
        let version = self.parse_version()?;
        let target = self.parse_target()?;
        let address_size = self.parse_address_size()?;
        let mut directives = vec![];
        while !self.is_eof() {
            let directive = self.parse_directive()?;
            directives.push(directive);
        }
        Ok(Module {
            version,
            target,
            address_size,
            directives,
        })
    }

    fn parse_directive(&mut self) -> ParseResult<Directive> {
        let mut tokens = vec![];
        loop {
            let (token, span) = self.next_token("directive")?;
            // Simple directives
            if let Token::Loc = token {
                return Ok(Directive::Loc);
            } else if let Token::Section = token {
                return self.consume_section();
            } else if let Token::Pragma = token {
                let (name, _) = self.parse_string()?;
                self.consume_if_match(Token::Semicolon)?;
                return Ok(Directive::Pragma(name));
            }

            tokens.push((token.clone(), span));
            // .entry or .func
            if token.is_function() {
                return Ok(Directive::Function(self.parse_function(tokens)?));
            }
            if let Token::Identifier(_) = token {
                return Ok(Directive::Variable(
                    self.parse_variable_declaration(tokens)?,
                ));
            }
        }
    }

    fn consume_section(&mut self) -> ParseResult<Directive> {
        loop {
            let (token, _) = self.next_token("section")?;
            if token == Token::RightBrace {
                break;
            }
        }
        Ok(Directive::Section)
    }

    fn parse_function(&mut self, tokens: Vec<(Token, Span)>) -> ParseResult<Function> {
        let (name, _) = self.parse_identifier()?;
        self.consume_or_error(Token::LeftParen)?;
        let mut parameters = vec![];
        // try to parse function arguments
        while self.consume_if_match(Token::RightParen)?.is_none() {
            let parameter = self.parse_parameter()?;
            parameters.push(parameter);
        }
        // ignore xxx
        while self.consume_if_match(Token::LeftBrace)?.is_none() {
            self.next_token("function body")?;
        }

        let mut body = vec![];
        while self.consume_if_match(Token::RightBrace)?.is_none() {
            let statement = self.parse_statement()?;
            body.push(statement);
        }
        let func = Function {
            link_directive: Some(LinkingDirective::Extern),
            entry: false,    // assuming default value for now
            noreturn: false, // assuming default value for now
            name,
            parameters,
            body,
        };
        self.build_function(func, tokens)
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        let token = self.cur_token()?;
        if token.is_directive() {
            let directive = self.parse_directive()?;
            Ok(Statement::Directive(directive))
        } else if let Token::Identifier(name) = token {
            // label
            self.next_token("label")?;
            self.consume_or_error(Token::Colon)?;
            Ok(Statement::Label(name))
        } else {
            let instruction = self.parse_instruction()?;
            Ok(Statement::Instruction(instruction))
        }
    }

    fn parse_parameter(&mut self) -> ParseResult<Parameter> {
        // TODO: we assume the Token::Param is always present first
        self.consume_or_error(Token::Param)?;
        let (token, span) = self.next_token("type")?;
        let ty = self.parse_type(token, span, "funtion parameter")?;
        let mut directives = vec![];
        while self.consume_if_match(Token::Comma)?.is_none() && !self.if_match(Token::RightParen)? {
            directives.push(self.next_token("parameter_name")?);
        }
        let (name, span) = directives.pop().unwrap();
        let name = if let Token::Identifier(name) = name {
            name
        } else {
            return Err(Diagnostic::error()
                .with_message(format!("expected `identifier`, found `{}`", name))
                .with_labels(vec![Label::primary(self.file_id, span)]));
        };
        let parameter = Parameter {
            state_space: None,
            ptr: false,
            alignment: None,
            name,
            ty,
        };
        self.build_parameter(parameter, directives)
    }

    fn parse_type(&self, token: Token, span: Span, error_msg: &str) -> ParseResult<Type> {
        match token {
            Token::F16 => Ok(Type::F16),
            Token::F16x2 => Ok(Type::F16x2),
            Token::F32 => Ok(Type::F32),
            Token::F64 => Ok(Type::F64),
            Token::S8 => Ok(Type::S8),
            Token::S16 => Ok(Type::S16),
            Token::S32 => Ok(Type::S32),
            Token::S64 => Ok(Type::S64),
            Token::U8 => Ok(Type::U8),
            Token::U16 => Ok(Type::U16),
            Token::U32 => Ok(Type::U32),
            Token::U64 => Ok(Type::U64),
            Token::B8 => Ok(Type::B8),
            Token::B16 => Ok(Type::B16),
            Token::B32 => Ok(Type::B32),
            Token::B64 => Ok(Type::B64),
            Token::B128 => Ok(Type::B128),
            Token::Bf16 => Ok(Type::Bf16),
            Token::Tf32 => Ok(Type::Tf32),
            Token::Pred => Ok(Type::Pred),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected `type` in {error_msg}, found `{}`", token))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    /// Parse a variable declaration
    /// assume the directive and name is already parsed
    fn parse_variable_declaration(
        &mut self,
        mut tokens: Vec<(Token, Span)>,
    ) -> ParseResult<VariableDecl> {
        let (name, span) = tokens.pop().unwrap();
        if let Token::Identifier(name) = name {
            let mut variable = VariableDecl {
                name,
                ty: Type::U32,
                state_space: None,
                linkage: None,
                vector: None,
                alignment: None,
                array: None,
                init: None,
                span,
            };
            variable = self.build_variable_prefix(variable, tokens)?;
            // check if %p<...> is present
            if (self.consume_if_match(Token::LessThan)?).is_some() {
                let (token, _) = self.parse_integer()?;
                variable.vector = Some(token as u32);
                self.consume_or_error(Token::GreaterThan)?;
            }
            if self.consume_if_match(Token::LeftBracket)?.is_some() {
                let num = if self.consume_if_match(Token::RightBracket)?.is_none() {
                    let (token, _) = self.parse_integer()?;
                    variable.array = Some(token as u32);
                    self.consume_or_error(Token::RightBracket)?;
                    token as u32
                } else {
                    0
                };
                variable.array = Some(num);
            }
            // initialize
            if self.consume_if_match(Token::Assign)?.is_some() {
                let init = self.parse_operand()?;
                variable.init = Some(init);
            }
            self.consume_or_error(Token::Semicolon)?;

            Ok(variable)
        } else {
            Err(Diagnostic::error()
                .with_message(format!("expected `identifier`, found `{}`", name))
                .with_labels(vec![Label::primary(self.file_id, span)]))
        }
    }

    fn parse_instruction(&mut self) -> ParseResult<Instruction> {
        let predicate = if self.consume_if_match(Token::At)?.is_some() {
            Some(self.parse_predicate()?)
        } else {
            None
        };

        let (opcode, opcode_span) = self.parse_opcode()?;
        let mut operands = vec![];
        while self.consume_if_match(Token::Semicolon)?.is_none() {
            let operand = self.parse_operand()?;
            operands.push(operand);
            self.consume_if_match(Token::Comma)?;
        }

        Ok(Instruction {
            predicate,
            opcode,
            operands,
            span: opcode_span,
        })
    }

    /// Parse a predicate, assuming `@` is already consumed
    fn parse_predicate(&mut self) -> ParseResult<Predicate> {
        let negated = self.consume_if_match(Token::Not)?.is_some();
        let (token, span) = self.next_token("predicate")?;
        let register = self.parse_register(token, span)?;
        Ok(Predicate { register, negated })
    }

    fn parse_register(&mut self, token: Token, span: Span) -> ParseResult<Register> {
        match token {
            Token::Identifier(name) => Ok(Register::Identifier(name, span)),
            Token::TId => Ok(Register::Special(SpecialReg::ThreadId, span)),
            Token::CTAId => Ok(Register::Special(SpecialReg::CtaId, span)),
            Token::CTAIdX => Ok(Register::Special(SpecialReg::CtaIdX, span)),
            Token::CTAIdY => Ok(Register::Special(SpecialReg::CtaIdY, span)),
            Token::CTAIdZ => Ok(Register::Special(SpecialReg::CtaIdZ, span)),
            Token::TIdX => Ok(Register::Special(SpecialReg::ThreadIdX, span)),
            Token::TIdY => Ok(Register::Special(SpecialReg::ThreadIdY, span)),
            Token::TIdZ => Ok(Register::Special(SpecialReg::ThreadIdZ, span)),
            Token::NTId => Ok(Register::Special(SpecialReg::NumThread, span)),
            Token::NTIdX => Ok(Register::Special(SpecialReg::NumThreadX, span)),
            Token::NTIdY => Ok(Register::Special(SpecialReg::NumThreadY, span)),
            Token::NTIdZ => Ok(Register::Special(SpecialReg::NumThreadZ, span)),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected `register`, found `{}`", token))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    fn parse_operand(&mut self) -> ParseResult<Operand> {
        if self.consume_if_match(Token::LeftBracket)?.is_some() {
            let operand = self.parse_address_operand()?;
            self.consume_or_error(Token::RightBracket)?;
            Ok(Operand::Address(operand))
        } else if self.consume_if_match(Token::LeftBrace)?.is_some() {
            let mut elements = vec![];
            while self.consume_if_match(Token::RightBrace)?.is_none() {
                let operand = self.parse_operand()?;
                elements.push(operand);
                self.consume_if_match(Token::Comma)?;
            }
            Ok(Operand::Vector(VectorOperand { elements }))
        } else {
            let (token, span) = self.next_token("operand")?;
            if let Ok(constant) = self.parse_constant(token.clone(), span.clone()) {
                Ok(Operand::Constant(constant))
            } else {
                let register = self.parse_register(token, span)?;
                Ok(Operand::Register(register))
            }
        }
    }

    fn parse_constant(&self, token: Token, span: Span) -> ParseResult<Constant> {
        match token {
            Token::DecimalConstant(number) => Ok(Constant::Integer(number)),
            Token::HexConstant(number) => {
                Ok(Constant::Integer(number as i64)) // TODO: check if this is correct
            }
            Token::OctalConstant(number) => Ok(Constant::Integer(number as i64)),
            Token::SingleConstant(number) => Ok(Constant::Float(number as f64)),
            Token::DoubleConstant(number) => Ok(Constant::Float(number)),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected `constant`, found `{}`", token))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    fn parse_address_operand(&mut self) -> ParseResult<AddressOperand> {
        // assume there is identifier
        let (identifier, _) = self.parse_identifier()?;
        if self.if_match(Token::RightBracket)? {
            Ok(AddressOperand::Address(identifier))
        } else {
            self.consume_or_error(Token::Plus)?;
            let (number, _) = self.parse_integer()?;
            Ok(AddressOperand::AddressOffset(identifier, number))
        }
    }

    /// parse an opcode and the corresponding type/directive
    fn parse_opcode(&mut self) -> ParseResult<(Opcode, Span)> {
        let (token, span) = self.next_token("opcode")?;
        let opcode = match token {
            Token::Mov => Opcode::Mov(self.next_type("opcode")?),
            Token::Mul => {
                let mode = self.parse_mul_mode()?;
                let ty = self.next_type("opcode")?;
                let rounding = self.parse_round_mode()?;
                let ftz = self.consume_if_match(Token::Ftz)?.is_some();
                let saturate = self.consume_if_match(Token::Saturate)?.is_some();
                Opcode::Mul {
                    ty,
                    mode,
                    rounding,
                    ftz,
                    saturate,
                }
            }
            Token::Add => Opcode::Add(self.next_type("opcode")?),
            Token::Sub => Opcode::Sub(self.next_type("opcode")?),
            Token::Div => {
                self.consume_if_match(Token::Full)?;
                let ftz = self.consume_if_match(Token::Ftz)?.is_some();
                let ty = self.next_type("opcode")?;
                Opcode::Div { ftz, ty }
            }
            Token::Shl => Opcode::Shl(self.next_type("opcode")?),
            Token::Shr => Opcode::Shr(self.next_type("opcode")?),
            Token::Ld => {
                // TODO: correct this
                let ty = loop {
                    let (token, span) = self.next_token("opcode")?;
                    if let Ok(ty) = self.parse_type(token, span, "opcode") {
                        break ty;
                    }
                };
                Opcode::Ld(ty)
            }
            Token::St => {
                // TODO: correct this
                let ty = loop {
                    let (token, span) = self.next_token("opcode")?;
                    if let Ok(ty) = self.parse_type(token, span, "opcode") {
                        break ty;
                    }
                };
                Opcode::St(ty)
            }
            Token::And => Opcode::And(self.next_type("opcode")?),
            Token::Or => Opcode::Or(self.next_type("opcode")?),
            Token::Xor => Opcode::XOr(self.next_type("opcode")?),
            Token::Setp => Opcode::Setp(self.parse_predicate_op()?, self.next_type("opcode")?),
            Token::SelP => Opcode::Selp(self.next_type("opcode")?),
            Token::Ret => Opcode::Ret,
            Token::Bar => {
                // TODO: fix
                let sync = self.consume_if_match(Token::Sync)?.is_some();
                let thread = self.parse_integer()?.0 as u32;
                Opcode::Bar { thread, sync }
            }
            Token::Max => Opcode::Max(self.next_type("opcode")?),
            Token::Shfl => {
                self.consume_or_error(Token::Sync)?;
                // TODO: fix mode
                self.next_token("opcode")?;
                let ty = self.next_type("opcode")?;
                Opcode::Shfl(ty)
            }
            Token::Neg => Opcode::Neg(self.next_type("opcode")?),
            Token::Fma => {
                let rounding = self.parse_round_mode()?;
                let ftz = self.consume_if_match(Token::Ftz)?.is_some();
                let ty = self.next_type("opcode")?;
                Opcode::Fma { rounding, ftz, ty }
            }
            Token::Ex2 => {
                self.consume_or_error(Token::Approx)?;
                let ftz = self.consume_if_match(Token::Ftz)?.is_some();
                let ty = self.next_type("opcode")?;
                Opcode::Ex2 { ftz, ty }
            }
            Token::Mma => {
                self.consume_or_error(Token::Sync)?;
                self.consume_or_error(Token::Aligned)?;
                let shape = self.parse_shape3()?;
                self.consume_or_error(Token::Row)?;
                self.consume_or_error(Token::Col)?;
                let dtype = self.next_type("opcode")?;
                let atype = self.next_type("opcode")?;
                let btype = self.next_type("opcode")?;
                let ctype = self.next_type("opcode")?;
                Opcode::Mma {
                    atype,
                    btype,
                    ctype,
                    dtype,
                    shape,
                }
            }
            // float: mad.rnd{.ftz}{.sat}.f32  d, a, b, c;
            // integer: mad.mode{.sat}.s32  d, a, b, c;
            Token::Mad => {
                let mode = self.parse_mul_mode()?;
                let rounding = self.parse_round_mode()?;
                let ftz = self.consume_if_match(Token::Ftz)?.is_some();
                let sat = self.consume_if_match(Token::Saturate)?.is_some();
                let ty = self.next_type("opcode")?;
                Opcode::Mad {
                    mode,
                    rounding,
                    ftz,
                    sat,
                    ty,
                }
            }
            // ldmatrix.sync.aligned.shape.num{.trans}{.ss}.type r, [p];
            Token::LdMatrix => {
                self.consume_or_error(Token::Sync)?;
                self.consume_or_error(Token::Aligned)?;
                let shape = self.parse_shape2()?;
                let xnum = self.parse_xnum()?;
                self.consume_if_match(Token::Trans)?;
                let shared = self.consume_if_match(Token::Shared)?.is_some();
                let ty = self.next_type("ldmatrix")?;
                Opcode::LdMatrix {
                    shape,
                    ty,
                    xnum,
                    shared,
                }
            }
            Token::Bra => Opcode::Bra,
            Token::Cp => {
                // ignore all directives
                let mut cur_token = self.cur_token()?;
                while cur_token.is_directive() {
                    self.next_token("opcode")?;
                    cur_token = self.cur_token()?;
                }
                Opcode::Cp
            }
            Token::Cvt => {
                let rounding = self.parse_round_mode()?;
                let from = self.next_type("opcode")?;
                let to = self.next_type("opcode")?;
                Opcode::Cvt { from, to, rounding }
            }
            Token::Cvta => {
                let to = self.consume_if_match(Token::To)?.is_some();
                let state_space = self.parse_state_space()?;
                let size = self.next_type("opcode")?;
                Opcode::Cvta {
                    to,
                    state_space,
                    size,
                }
            }
            Token::Bfe => Opcode::Bfe(self.next_type("opcode")?),
            _ => {
                return Err(Diagnostic::error()
                    .with_message(format!("expected opcode, found `{}`", token))
                    .with_labels(vec![Label::primary(self.file_id, span)]));
            }
        };
        Ok((opcode, span))
    }

    fn parse_shape2(&mut self) -> ParseResult<Shape2> {
        let (next, span) = self.next_token("shape2")?;
        match next {
            Token::M8N8 => Ok(Shape2::M8N8),
            Token::M8N16 => Ok(Shape2::M8N16),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected m.n., found `{}`", next))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    fn parse_shape3(&mut self) -> ParseResult<Shape3> {
        let (next, span) = self.next_token("shape3")?;
        match next {
            Token::M16N8K16 => Ok(Shape3::M16N8K16),
            Token::M16N8K32 => Ok(Shape3::M16N8K32),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected m.n.k., found `{}`", next))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    fn parse_string(&mut self) -> ParseResult<(String, Span)> {
        let (token, span) = self.next_token("string")?;
        match token {
            Token::String(string) => Ok((string, span)),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected `string`, found `{}`", token))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    fn parse_xnum(&mut self) -> ParseResult<u32> {
        let (next, span) = self.next_token("num")?;
        match next {
            Token::X4 => Ok(4),
            Token::X1 => Ok(1),
            Token::X2 => Ok(2),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected number, found `{}`", next))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    fn parse_round_mode(&mut self) -> ParseResult<Option<FloatRoundingMode>> {
        let token = self.cur_token()?;
        match token {
            Token::Rn | Token::Rna | Token::Rz | Token::Rm | Token::Rp => {
                self.next_token("round_mode")?;
                Ok(Some(token.into()))
            }
            _ => Ok(None),
        }
    }

    fn parse_predicate_op(&mut self) -> ParseResult<PredicateOp> {
        let (token, span) = self.next_token("predicate_op")?;
        match token {
            Token::Le => Ok(PredicateOp::LessEqual),
            Token::Lt => Ok(PredicateOp::LessThan),
            Token::Eq => Ok(PredicateOp::Equal),
            Token::Ne => Ok(PredicateOp::NotEqual),
            Token::Gt => Ok(PredicateOp::GreaterThan),
            Token::Ge => Ok(PredicateOp::GreaterEqual),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected `predicate_op`, found `{}`", token))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    fn next_type(&mut self, error_msg: &str) -> ParseResult<Type> {
        let (token, span) = self.next_token("type")?;
        self.parse_type(token, span, error_msg)
    }

    fn parse_version(&mut self) -> ParseResult<Version> {
        self.consume_or_error(Token::Version)?;
        let major = self.parse_integer()?.0 as u32;
        self.consume_or_error(Token::Dot)?;
        let minor = self.parse_integer()?.0 as u32;
        Ok(Version { major, minor })
    }

    fn parse_target(&mut self) -> ParseResult<Target> {
        self.consume_or_error(Token::Target)?;
        let (name, _) = self.parse_identifier()?;
        Ok(Target(name))
    }

    fn parse_identifier(&mut self) -> ParseResult<(String, Span)> {
        let (token, span) = self.next_token("identifier")?;
        match token {
            Token::Identifier(identifier) => Ok((identifier, span)),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected `identifier`, found `{}`", token))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    fn parse_mul_mode(&mut self) -> ParseResult<Option<MulMode>> {
        let token = self.cur_token()?;
        match token {
            Token::Hi | Token::Lo | Token::Wide => {
                self.next_token("mul_mode")?;
                Ok(Some(token.into()))
            }
            _ => Ok(None),
        }
    }

    fn parse_address_size(&mut self) -> ParseResult<AddressSize> {
        self.consume_or_error(Token::AddressSize)?;
        let (token, span) = self.next_token("constant")?;
        match token {
            Token::DecimalConstant(64) => Ok(AddressSize::Bits64),
            Token::DecimalConstant(32) => Ok(AddressSize::Bits32),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected `32` or `64`, found `{}`", token))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    fn parse_integer(&mut self) -> ParseResult<(i64, Span)> {
        let (token, span) = self.next_token("number")?;
        match token {
            Token::DecimalConstant(number) => Ok((number, span)),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected `number`, found `{}`", token))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    fn next_token(&mut self, error_msg: &str) -> ParseResult<(Token, Span)> {
        match self.tokens.next() {
            Some((token, span)) => match token {
                Ok(token) => Ok((token, span)),
                Err(_) => Err(Diagnostic::error()
                    .with_message("lexer error")
                    .with_labels(vec![Label::primary(self.file_id, span)])),
            },
            None => {
                Err(Diagnostic::error()
                    .with_message(format!("expected `{}`, found EOF", error_msg)))
            }
        }
    }

    fn is_eof(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    /// Consumes the next token and return the span if it is the expected token,
    /// otherwise returns an error.
    fn consume_or_error(&mut self, token: Token) -> ParseResult<Span> {
        let (next, span) = self.next_token(&token.to_string())?;
        if next == token {
            Ok(span)
        } else {
            Err(Diagnostic::error()
                .with_message(format!("expected `{}`, found `{}`", token, next))
                .with_labels(vec![Label::primary(self.file_id, span)]))
        }
    }

    /// Check if the next token is the expected token, if so consume it and return the span,
    /// otherwise returns None.
    fn consume_if_match(&mut self, token: Token) -> ParseResult<Option<Span>> {
        let (cur_token, span) = self
            .tokens
            .peek()
            .ok_or_else(|| {
                Diagnostic::error().with_message(format!("expected `{}`, found EOF", token))
            })?
            .clone();
        match cur_token {
            Ok(cur_token) if cur_token == token => {
                self.tokens.next();
                Ok(Some(span))
            }
            _ => Ok(None),
        }
    }

    fn cur_token(&mut self) -> ParseResult<Token> {
        let (cur_token, _) = self
            .tokens
            .peek()
            .ok_or_else(|| Diagnostic::error().with_message("expected token, found EOF"))?
            .clone();
        match cur_token {
            Ok(cur_token) => Ok(cur_token),
            _ => Err(Diagnostic::error().with_message("lexer error")),
        }
    }

    fn if_match(&mut self, token: Token) -> ParseResult<bool> {
        let (cur_token, _) = self
            .tokens
            .peek()
            .ok_or_else(|| {
                Diagnostic::error().with_message(format!("expected `{}`, found EOF", token))
            })?
            .clone();
        match cur_token {
            Ok(cur_token) => Ok(cur_token == token),
            _ => Ok(false),
        }
    }

    fn parse_state_space(&mut self) -> ParseResult<StateSpace> {
        let (token, span) = self.next_token("state_space")?;
        match token {
            Token::Reg => Ok(StateSpace::Reg),
            Token::Const => Ok(StateSpace::Const),
            Token::Local => Ok(StateSpace::Local),
            Token::Param => Ok(StateSpace::Param),
            Token::SReg => Ok(StateSpace::SReg),
            Token::Tex => Ok(StateSpace::Tex),
            Token::Global => Ok(StateSpace::Global),
            Token::Shared => Ok(StateSpace::Shared),
            _ => Err(Diagnostic::error()
                .with_message(format!("expected state_space, found `{}`", token))
                .with_labels(vec![Label::primary(self.file_id, span)])),
        }
    }

    fn build_parameter(
        &self,
        mut parameter: Parameter,
        tokens: Vec<(Token, Span)>,
    ) -> ParseResult<Parameter> {
        for (token, span) in tokens {
            match token {
                Token::Ptr => {
                    parameter.ptr = true;
                }
                Token::Global => {
                    parameter.state_space = Some(StateSpace::Global);
                }
                Token::Align(align) => {
                    parameter.alignment = Some(align);
                }
                _ => {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "unexpected token, expected directive for parameters, found {}",
                            token
                        ))
                        .with_labels(vec![Label::primary(self.file_id, span)]));
                }
            }
        }
        Ok(parameter)
    }

    fn build_function(
        &self,
        mut func: Function,
        tokens: Vec<(Token, Span)>,
    ) -> ParseResult<Function> {
        for (token, span) in tokens {
            match token {
                Token::Entry => {
                    func.entry = true;
                }
                Token::Extern => {
                    func.link_directive = Some(LinkingDirective::Extern);
                }
                Token::Visible => {
                    func.link_directive = Some(LinkingDirective::Visible);
                }
                Token::Weak => {
                    func.link_directive = Some(LinkingDirective::Weak);
                }
                Token::Common => {
                    func.link_directive = Some(LinkingDirective::Common);
                }
                _ => {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "unexpected token, expected directive for function, found {}",
                            token
                        ))
                        .with_labels(vec![Label::primary(self.file_id, span)]));
                }
            }
        }
        Ok(func)
    }

    fn build_variable_prefix(
        &self,
        mut variable: VariableDecl,
        tokens: Vec<(Token, Span)>,
    ) -> ParseResult<VariableDecl> {
        for (token, span) in tokens {
            match token {
                Token::Global => {
                    variable.state_space = Some(StateSpace::Global);
                }
                Token::Reg => {
                    variable.state_space = Some(StateSpace::Reg);
                }
                Token::Shared => {
                    variable.state_space = Some(StateSpace::Shared);
                }
                Token::Extern => {
                    variable.linkage = Some(LinkingDirective::Extern);
                }
                Token::Align(align) => {
                    variable.alignment = Some(align);
                }
                _ => {
                    // assume it is a type
                    let ty = self.parse_type(token, span, "variable declaration")?;
                    variable.ty = ty;
                }
            }
        }
        Ok(variable)
    }
}
