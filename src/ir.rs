use codespan_reporting::diagnostic::Diagnostic;
use logos::Span;

use crate::{lexer::Token, parser::Parser};

/// PTX module
#[derive(Debug)]
pub struct Module {
    pub version: Version,
    pub target: Target,
    pub address_size: AddressSize,
    pub directives: Vec<Directive>,
}
/// A PTX statement is either a directive or an instruction.
/// Statements begin with an optional label and end with a semicolon.
#[derive(Debug)]
pub enum Statement {
    Instruction(Instruction),
    Directive(Directive),
    Label(String),
}

/// PTX instructions generally have from zero to four operands,
/// plus an optional guard predicate appearing after an @ symbol to the left of the opcode:
#[derive(Debug)]
pub struct Instruction {
    pub predicate: Option<Predicate>,
    pub opcode: Opcode,
    pub operands: Vec<Operand>,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub enum Directive {
    Function(Function),
    Variable(VariableDeclaration),
    Loc,
    Section,
}

#[derive(Debug)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
}

#[derive(Debug)]
pub struct Target(pub String);

#[derive(Debug)]
pub enum AddressSize {
    Bits32,
    Bits64,
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub name: String,
    pub state_space: Option<StateSpace>,
    pub linkage: Option<LinkingDirective>,
    pub ty: Type,
    pub alignment: Option<u32>,
    pub vector: Option<u32>,
    pub(crate) span: Span,
}

#[derive(Debug)]
pub enum StateSpace {
    /// Registers, fast
    Reg,
    /// Special registers. Read-only; pre-defined; platform-specific.
    SReg,
    /// Shared, read-only memory.
    Const,
    /// Global memory, shared by all threads.
    Global,
    /// Local memory, private to each thread.
    Local,
    /// Kernel parameters, defined per-grid; or
    /// Function or local parameters, defined per-thread.
    Param,
    /// Addressable memory, defined per CTA, accessible to all threads in the cluster throughout the lifetime of the CTA that defines it.
    Shared,
    /// Global texture memory (deprecated).
    Tex,
}

#[derive(Debug)]
pub enum LinkingDirective {
    /// External symbol declaration.
    Extern,
    /// Visible (externally) symbol declaration.
    Visible,
    /// Visible (externally) symbol declaration.
    Weak,
    /// Visible (externally) symbol declaration.
    Common,
}

#[derive(Debug)]
pub enum Type {
    B8,
    B16,
    B32,
    B64,
    S8,
    S16,
    S32,
    S64,
    U8,
    U16,
    U32,
    U64,
    F16,
    F16x2,
    F32,
    F64,
    Tf32,
    Bf16,
    Pred,
    B128,
    Ptr(Box<Type>),
}

#[derive(Debug)]
pub struct Function {
    pub link_directive: Option<LinkingDirective>,
    pub entry: bool,
    pub noreturn: bool,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
    pub ptr: bool,
    pub state_space: Option<StateSpace>,
    pub alignment: Option<u32>,
}

#[derive(Debug)]
pub enum Opcode {
    Ex2 {
        ty: Type,
        /// flush-to-zero
        ftz: bool,
    },
    Fma {
        rounding: Option<RoundingMode>,
        ty: Type,
        ftz: bool,
    },
    Neg(Type),
    Max(Type),
    Shfl(Type),
    Bar {
        thread: u32,
        sync: bool,
    },
    Cvt {
        from: Type,
        to: Type,
        rounding: Option<RoundingMode>,
    },
    Cp,
    Ret,
    Mov(Type),
    Add(Type),
    Sub(Type),
    Mul {
        ty: Type,
        mode: Option<MulMode>,
        rounding: Option<RoundingMode>,
        ftz: bool,
        saturate: bool,
    },
    Div {
        ty: Type,
        ftz: bool,
    },
    Shl(Type),
    Shr(Type),
    SetpLt,
    SetpGt,
    SetpEq,
    Ld(Type),
    LdMatrix {
        direction: Vec<Token>,
        ty: Type,
    },
    Mma {
        shape: Shape,
        atype: Type,
        btype: Type,
        ctype: Type,
        dtype: Type,
    },
    St(Type),
    And(Type),
    Or(Type),
    XOr(Type),
    Setp(PredicateOp, Type),
    Selp(Type),
    Bfe(Type),
    Bra,
}

/// Operands may be
/// - register variables,
/// - constant expressions,
/// - address expressions,
/// - label names.
#[derive(Debug)]
pub enum Operand {
    Register(Register),
    Constant(Constant),
    Address(AddressOperand),
    Vector(VectorOperand),
    Label(String),
}

#[derive(Debug)]
pub enum Register {
    Special(SpecialReg, Span),
    Identifier(String, Span),
}

#[derive(Debug)]
pub enum SpecialReg {
    StackPtr,
    ThreadId,
    ThreadIdX,
    ThreadIdY,
    ThreadIdZ,
    NumThread,
    NumThreadX,
    NumThreadY,
    NumThreadZ,
    CtaId,
    CtaIdX,
    CtaIdY,
    CtaIdZ,
    NumCta,
    NumCtaX,
    NumCtaY,
    NumCtaZ,
}

#[derive(Debug)]
pub enum Constant {
    Integer(i64),
    Float(f64),
}

#[derive(Debug, Clone)]
pub enum AddressOperand {
    /// the name of an addressable variable var.
    Address(String),
    /// a sum of register reg containing a byte address plus a constant integer byte offset (signed, 32-bit).
    AddressOffset(String, i64),
    /// an immediate absolute byte address (unsigned, 32-bit).
    Immediate(u32),
    /// an array element
    ArrayIndex(String, usize),
}

#[derive(Debug)]
pub struct VectorOperand {
    pub elements: Vec<Operand>,
}

#[derive(Debug)]
pub struct Predicate {
    pub register: Register,
    pub negated: bool,
}

#[derive(Debug)]
pub enum RoundingMode {
    /// Round to nearest even
    Rn,
    /// Round towards zero
    Rna,
    Rz,
    Rm,
    Rp,
}

#[derive(Debug)]
pub enum MulMode {
    Hi,
    Lo,
    Wide,
}

#[derive(Debug, Clone, Copy)]
pub enum PredicateOp {
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equal,
    NotEqual,
}

#[derive(Debug)]
pub enum Shape {
    M16n8k16,
    M16n8k32,
}

impl Module {
    pub fn from_ptx(source: &str, file_id: usize) -> Result<Self, Diagnostic<usize>> {
        let mut parser = Parser::new(file_id, source);
        parser.parse()
    }
}

impl From<Token> for MulMode {
    fn from(token: Token) -> Self {
        match token {
            Token::Hi => MulMode::Hi,
            Token::Lo => MulMode::Lo,
            Token::Wide => MulMode::Wide,
            _ => unreachable!(),
        }
    }
}

impl From<Token> for RoundingMode {
    fn from(token: Token) -> Self {
        match token {
            Token::Rn => RoundingMode::Rn,
            Token::Rna => RoundingMode::Rna,
            Token::Rz => RoundingMode::Rz,
            Token::Rm => RoundingMode::Rm,
            Token::Rp => RoundingMode::Rp,
            _ => unreachable!(),
        }
    }
}

impl From<Token> for Shape {
    fn from(token: Token) -> Self {
        match token {
            Token::M16N8K16 => Shape::M16n8k16,
            Token::M16N8K32 => Shape::M16n8k32,
            _ => unreachable!(),
        }
    }
}
