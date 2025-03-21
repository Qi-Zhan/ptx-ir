//! Intermediate representation of PTX code.

use codespan_reporting::{diagnostic::Diagnostic, files::SimpleFiles, term::termcolor::Buffer};
use either::Either;
use logos::Span;
use std::path::Path;

use crate::{lexer::Token, parser::Parser};

/// PTX module, the top-level structure in a PTX file.
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
    Block(Vec<Statement>),
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
    pub span: Span,
}

#[derive(Debug)]
pub enum Directive {
    Function(Function),
    FunctionDecl(Function),
    Variable(VariableDecl),
    Loc,
    Section,
    Pragma(String),
    File(usize, String),
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
pub struct VariableDecl {
    pub name: String,
    pub state_space: Option<StateSpace>,
    pub linkage: Option<LinkingDirective>,
    pub ty: Type,
    pub alignment: Option<u32>,
    pub vector: Option<u32>,
    pub array: Option<u32>,
    pub init: Option<Operand>,
    pub span: Span,
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
}

#[derive(Debug)]
pub struct Function {
    pub link_directive: Option<LinkingDirective>,
    pub entry: bool,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_params: Vec<Parameter>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
    pub array: Option<u32>,
    pub ptr: bool,
    pub state_space: Option<StateSpace>,
    pub alignment: Option<u32>,
}

#[derive(Debug)]
pub enum Opcode {
    Add {
        ty: Type,
        saturate: bool,
        rnd: bool,
        ftz: bool,
    },
    Sub {
        ty: Type,
        ftz: bool,
    },
    /// Count leading zeros
    Clz(Type),
    Not(Type),
    /// Vote across thread group, Deprecated
    Vote(Type),
    /// Population count.
    PopC(Type),
    Exit,
    Call(CallInst),
    // CallPrototype {
    //     parameters: Vec<Parameter>,
    //     return_params: Vec<Parameter>,
    //     no_return: bool,
    // },
    CallPrototype {},
    Abs {
        ty: Type,
        ftz: bool,
    },
    Ex2 {
        ty: Type,
        /// flush-to-zero
        ftz: bool,
    },
    Rem(Type),
    Fma {
        rounding: Option<FloatRoundingMode>,
        ty: Type,
        ftz: bool,
    },
    Mad {
        mode: Option<MulMode>,
        rounding: Option<FloatRoundingMode>,
        ty: Type,
        ftz: bool,
        sat: bool,
    },
    Rcp {
        ty: Type,
        ftz: bool,
        approx: bool,
        rounding: Option<FloatRoundingMode>,
    },
    Sqrt {
        ty: Type,
        ftz: bool,
        approx: bool,
        rounding: Option<FloatRoundingMode>,
    },
    RSqrt {
        ty: Type,
        ftz: bool,
        approx: bool,
        rounding: Option<FloatRoundingMode>,
    },
    Sin {
        ty: Type,
        approx: bool,
        ftz: bool,
    },
    Cos {
        ty: Type,
        approx: bool,
        ftz: bool,
    },
    Neg {
        ty: Type,
        ftz: bool,
    },
    Max(Type),
    Min(Type),
    Shfl(Type),
    Bar {
        thread: u32,
        sync: bool,
    },
    Cvt {
        from: Type,
        to: Type,
        rounding: Option<Either<FloatRoundingMode, IntegerRoundingMode>>,
        saturate: bool,
    },
    Cvta {
        to: bool,
        state_space: StateSpace,
        size: Type,
    },
    Cp,
    Ret,
    Mov(Type),
    Lg2 {
        ty: Type,
        ftz: bool,
    },
    Mul {
        ty: Type,
        mode: Option<MulMode>,
        rounding: Option<FloatRoundingMode>,
        ftz: bool,
        saturate: bool,
    },
    Mul24 {
        ty: Type,
        mode: Option<MulMode>,
    },
    Div {
        approx: bool,
        full: bool,
        rounding: Option<FloatRoundingMode>,
        ty: Type,
        ftz: bool,
    },
    Shl(Type),
    Shr(Type),
    Shf {
        direction: ShfDirection,
        mode: ShfMode,
    },
    SetpLt,
    SetpGt,
    SetpEq,
    Ld(Type),
    Ldu(Type),
    Tex(Type, Type),
    /// Select one source operand, based on the sign of the third operand.
    Slct {
        ftz: bool,
        dtype: Type,
        stype: Type,
    },
    /// Atomic reduction operations for thread-to-thread communication.
    Atom(Type),
    LdMatrix {
        shape: Shape2,
        ty: Type,
        xnum: u32,
        shared: bool,
    },
    Mma {
        shape: Shape3,
        atype: Type,
        btype: Type,
        ctype: Type,
        dtype: Type,
    },
    St(Type),
    And(Type),
    Or(Type),
    XOr(Type),
    Set {
        cmp_op: PredicateOp,
        ftz: bool,
        dtype: Type,
        stype: Type,
    },
    Setp(PredicateOp, Type),
    Selp(Type),
    Bfe(Type),
    Bra,
    Membar,
    /// Query whether a generic address falls within a specified state space window
    IsSpaceP(StateSpace),
}

#[derive(Debug)]
pub struct CallInst {
    pub is_uniform: bool,
    pub return_operand: Option<Operand>,
    pub function: String,
    pub arguments: Vec<Operand>,
    pub fproto: Option<String>,
}

/// Operands may be
/// - register variables,
/// - constant expressions,
/// - address expressions,
/// - label names.
/// - place holders
#[derive(Debug)]
pub enum Operand {
    Register(Register),
    RegisterOffset(Register, i64),
    Constant(Constant),
    Address(AddressOperand),
    Vector(VectorOperand),
    Label(String),
    PlaceHolder,
}

#[derive(Debug)]
pub enum Register {
    Special(SpecialReg, Span),
    Identifier(String, Span),
}

#[derive(Debug)]
pub enum SpecialReg {
    StackPtr,
    Clock,

    ThreadId,
    ThreadIdX,
    ThreadIdY,
    ThreadIdZ,
    BlockDim,
    BlockDimX,
    BlockDimY,
    BlockDimZ,
    BlockIdx,
    BlockIdxX,
    BlockIdxY,
    BlockIdxZ,
    GridDim,
    GridDimX,
    GridDimY,
    GridDimZ,
}

#[derive(Debug)]
pub enum Constant {
    Integer(i64),
    Float(f64),
}

#[derive(Debug)]
pub enum AddressOperand {
    /// the name of an addressable variable var.
    Address(String),
    /// a sum of register reg containing a byte address plus a constant integer byte offset (signed, 32-bit).
    AddressOffset(String, i64),
    /// an immediate absolute byte address (unsigned, 32-bit).
    Immediate(u32),
    /// an array element
    ArrayIndex(String, usize),
    /// List
    List(String, Vec<Operand>),
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
pub enum FloatRoundingMode {
    /// Round to nearest even
    Rn,
    /// Round to nearest, ties away from zero
    Rna,
    /// Round towards zero
    Rz,
    /// Round towards -∞
    Rm,
    /// Round towards +∞
    Rp,
}

#[derive(Debug)]
pub enum IntegerRoundingMode {
    /// Round to nearest integer, choosing even integer if source is equidistant between two integers.
    Rni,
    /// Round to nearest integer in the direction of zero
    Rzi,
    /// Round to nearest integer in direction of negative infinity
    Rmi,
    /// Round to nearest integer in direction of positive infinity
    Rpi,
}

#[derive(Debug)]
pub enum MulMode {
    Hi,
    Lo,
    Wide,
}

#[derive(Debug)]
pub enum ShfDirection {
    Left,
    Right,
}

#[derive(Debug)]
pub enum ShfMode {
    Wrap,
    Clamp,
}

#[derive(Debug, Clone, Copy)]
pub enum PredicateOp {
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equal,
    NotEqual,
    EqualUnsigned,
    NotEqualUnsigned,
    LessThanUnsigned,
    LessEqualUnsigned,
    GreaterThanUnsigned,
    GreaterEqualUnsigned,
}

#[derive(Debug)]
pub enum Shape3 {
    M16N8K8,
    M16N8K16,
    M16N8K32,
}

#[derive(Debug)]
pub enum Shape2 {
    M8N8,
    M8N16,
    M16N8,
    M16N16,
}

impl Module {
    pub fn from_ptx(source: &str, file_id: usize) -> Result<Self, Diagnostic<usize>> {
        let mut parser = Parser::new(file_id, source);
        parser.parse()
    }

    pub fn from_ptx_path(path: &Path) -> Result<Self, String> {
        let mut files = SimpleFiles::new();
        let content = std::fs::read_to_string(path).expect("failed to read file");
        let file_id = files.add(
            path.to_str().expect("failed to convert path to str"),
            &content,
        );
        Self::from_ptx(&content, file_id).map_err(|diagnostic| emit_string(diagnostic, files))
    }
}

fn emit_string(diagnostic: Diagnostic<usize>, files: SimpleFiles<&str, &String>) -> String {
    let mut buffer = Buffer::ansi();
    let config = codespan_reporting::term::Config::default();
    codespan_reporting::term::emit(&mut buffer, &config, &files, &diagnostic).unwrap();
    String::from_utf8(buffer.into_inner()).unwrap()
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

impl From<Token> for FloatRoundingMode {
    fn from(token: Token) -> Self {
        match token {
            Token::Rn => FloatRoundingMode::Rn,
            Token::Rna => FloatRoundingMode::Rna,
            Token::Rz => FloatRoundingMode::Rz,
            Token::Rm => FloatRoundingMode::Rm,
            Token::Rp => FloatRoundingMode::Rp,
            _ => unreachable!(),
        }
    }
}

impl From<Token> for IntegerRoundingMode {
    fn from(token: Token) -> Self {
        match token {
            Token::Rni => IntegerRoundingMode::Rni,
            Token::Rzi => IntegerRoundingMode::Rzi,
            Token::Rmi => IntegerRoundingMode::Rmi,
            Token::Rpi => IntegerRoundingMode::Rpi,
            _ => unreachable!(),
        }
    }
}
