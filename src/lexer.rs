use std::fmt;

fn lex_align(lex: &mut Lexer<'_, Token>) -> u32 {
    let num_str = lex.slice().split_whitespace().nth(1).unwrap();
    num_str.parse().unwrap()
}

fn lex_hex(lex: &mut Lexer<'_, Token>) -> u64 {
    let num_str = lex.slice().split_at(2).1;
    u64::from_str_radix(num_str, 16).unwrap()
}

fn lex_octal(lex: &mut Lexer<'_, Token>) -> u64 {
    let num_str = lex.slice().split_at(1).1;
    u64::from_str_radix(num_str, 8).unwrap()
}

fn lex_single_float(lex: &mut Lexer<'_, Token>) -> f32 {
    let num_str = lex.slice().split_at(2).1;
    let bits = u32::from_str_radix(num_str, 16)
        .map_err(|e| format!("Failed to parse hex: {}", e))
        .unwrap();
    f32::from_bits(bits)
}

fn lex_double_float(lex: &mut Lexer<'_, Token>) -> f64 {
    let num_str = lex.slice().split_at(2).1;
    let bits = u64::from_str_radix(num_str, 16)
        .map_err(|e| format!("Failed to parse hex: {}", e))
        .unwrap();
    f64::from_bits(bits)
}

use logos::{Lexer, Logos};
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Ignore blank space
#[logos(skip r"//.*")] // Ignore comment
pub enum Token {
    // 4.3.1 Directives
    #[token(".address_size")]
    AddressSize,
    #[token(".explicitcluster")]
    ExplicitCluster,
    #[token(".maxnreg")]
    MaxNReg,
    #[token(".section")]
    Section,
    #[token(".alias")]
    Alias,
    #[token(".extern")]
    Extern,
    #[token(".maxntid")]
    MaxNTid,
    #[regex(r".align[ \t\f\n]+\d+", lex_align)]
    Align(u32),
    #[token(".file")]
    File,
    #[token(".minnctapersm")]
    MinNCTapersm,
    #[token(".branchtargets")]
    BranchTargets,
    #[token(".func")]
    Func,
    #[token(".noreturn")]
    NoReturn,
    #[token(".target")]
    Target,
    #[token(".callprototype")]
    CallPrototype,
    #[token(".calltargets")]
    CallTargets,
    /// .loc file_index line_number column_position
    /// TODO: .loc file_index line_number column_position,function_name label {+ immediate }, inlined_at file_index2 line_number2 column_position2
    #[regex(r".loc[ \t\f\n]+[0-9]+[ \t\f\n]+[0-9]+[ \t\f\n]+[0-9]+")]
    Loc,
    #[token(".pragma")]
    Pragma,
    #[token(".version")]
    Version,
    #[token(".common")]
    Common,
    #[token(".visible")]
    Visible,
    #[token(".maxclusterrank")]
    MaxClusterRank,
    #[token(".reqnctapercluster")]
    ReqNCTaperCluster,
    #[token(".weak")]
    Weak,
    #[token(".entry")]
    Entry,
    #[token(".maxnctapersm")]
    MaxNCTapersm,
    #[regex(".reqntid [0-9]+")]
    #[regex(".reqntid [0-9]+, [0-9]+")]
    #[regex(".reqntid [0-9]+, [0-9]+, [0-9]+")]
    ReqNTid,
    #[token(".ptr")]
    Ptr,
    #[token(".type")]
    Type,
    // 4.3.2 Reserved Instruction Keywords
    #[token("abs")]
    Abs,
    #[token("cvta")]
    Cvta,
    #[token("membar")]
    Membar,
    #[token("setp")]
    Setp,
    #[token("vabsdiff4")]
    VAbsDiff4,
    #[token("activemask")]
    ActiveMask,
    #[token("discard")]
    Discard,
    #[token("min")]
    Min,
    #[token("shf")]
    Shf,
    #[token("vadd")]
    VAdd,
    #[token("add")]
    Add,
    #[token("div")]
    Div,
    #[token("mma")]
    Mma,
    #[token("shfl")]
    Shfl,
    #[token("vadd2")]
    VAdd2,
    #[token("addc")]
    AddC,
    #[token("dp2a")]
    Dp2A,
    #[token("mov")]
    Mov,
    #[token("shl")]
    Shl,
    #[token("vadd4")]
    VAdd4,
    #[token("alloca")]
    Alloca,
    #[token("dp4a")]
    Dp4A,
    #[token("movmatrix")]
    MovMatrix,
    #[token("shr")]
    Shr,
    #[token("vavrg2")]
    VAverage2,
    #[token("and")]
    And,
    #[token("elect")]
    Elect,
    #[token("mul")]
    Mul,
    #[token("sin")]
    Sin,
    #[token("vavrg4")]
    VAverage4,
    #[token("applypriority")]
    ApplyPriority,
    #[token("ex2")]
    Ex2,
    #[token("mul24")]
    Mul24,
    #[token("slct")]
    Slct,
    #[token("vmad")]
    VMad,
    #[token("atom")]
    Atom,
    #[token("exit")]
    Exit,
    #[token("multimem")]
    MultiMem,
    #[token("sqrt")]
    Sqrt,
    #[token("vmax")]
    VMax,
    #[token("bar")]
    Bar,
    #[token("fence")]
    Fence,
    #[token("nanosleep")]
    NanoSleep,
    #[token("st")]
    St,
    #[token("vmax2")]
    VMax2,
    #[token("barrier")]
    Barrier,
    #[token("fma")]
    Fma,
    #[token("neg")]
    Neg,
    #[token("stackrestore")]
    StackRestore,
    #[token("vmax4")]
    VMax4,
    #[token("bfe")]
    Bfe,
    #[token("fns")]
    Fns,
    #[token("not")]
    Not,
    #[token("stacksave")]
    StackSave,
    #[token("vmin")]
    VMin,
    #[token("bfi")]
    Bfi,
    #[token("getctarank")]
    GetCtaRank,
    #[token("or")]
    Or,
    #[token("stmatrix")]
    StMatrix,
    #[token("vmin2")]
    VMin2,
    #[token("bfind")]
    BFind,
    #[token("griddepcontrol")]
    GridDepControl,
    #[token("pmevent")]
    PmEvent,
    #[token("sub")]
    Sub,
    #[token("vmin4")]
    VMin4,
    #[token("bmsk")]
    Bmsk,
    #[token("isspacep")]
    IsSpaceP,
    #[token("popc")]
    PopC,
    #[token("subc")]
    SubC,
    #[token("vote")]
    Vote,
    #[token("bra")]
    Bra,
    #[token("istypep")]
    IsTypeP,
    #[token("prefetch")]
    Prefetch,
    #[token("suld")]
    Suld,
    #[token("vset")]
    VSet,
    #[token("brev")]
    Brev,
    #[token("ld")]
    Ld,
    #[token("prefetchu")]
    PrefetchU,
    #[token("suq")]
    Suq,
    #[token("vset2")]
    VSet2,
    #[token("brkpt")]
    BrkPt,
    #[token("ldmatrix")]
    LdMatrix,
    #[token("prmt")]
    Prmt,
    #[token("sured")]
    Sured,
    #[token("vset4")]
    VSet4,
    #[token("brx")]
    Brx,
    #[token("ldu")]
    Ldu,
    #[token("rcp")]
    Rcp,
    #[token("sust")]
    Sust,
    #[token("vshl")]
    VShl,
    #[token("call")]
    Call,
    #[token("lg2")]
    Lg2,
    #[token("red")]
    Red,
    #[token("szext")]
    SzExt,
    #[token("vshr")]
    VShr,
    #[token("clz")]
    Clz,
    #[token("lop3")]
    Lop3,
    #[token("redux")]
    Redux,
    #[token("tanh")]
    Tanh,
    #[token("vsub")]
    VSub,
    #[token("cnot")]
    CNot,
    #[token("mad")]
    Mad,
    #[token("rem")]
    Rem,
    #[token("testp")]
    TestP,
    #[token("vsub2")]
    VSub2,
    #[token("copysign")]
    CopySign,
    #[token("mad24")]
    Mad24,
    #[token("ret")]
    Ret,
    #[token("vsub4")]
    VSub4,
    #[token("cos")]
    Cos,
    #[token("madc")]
    MadC,
    #[token("rsqrt")]
    RSqrt,
    #[token("tld4")]
    Tld4,
    #[token("wgmma")]
    WgMma,
    #[token("clusterlaunchcontrol")]
    ClusterLaunchControl,
    #[token("mapa")]
    MapA,
    #[token("sad")]
    Sad,
    #[token("trap")]
    Trap,
    #[token("wmma")]
    Wmma,
    #[token("cp")]
    Cp,
    #[token("match")]
    Match,
    #[token("selp")]
    SelP,
    #[token("txq")]
    TxQ,
    #[token("xor")]
    Xor,
    #[token("createpolicy")]
    CreatePolicy,
    #[token("max")]
    Max,
    #[token("set")]
    Set,
    #[token("vabsdiff")]
    VAbsDiff,
    #[token("cvt")]
    Cvt,
    #[token("mbarrier")]
    MBarrier,
    #[token("setmaxnreg")]
    SetMaxNReg,
    #[token("vabsdiff2")]
    VAbsDiff2,

    // Identifiers
    #[token("%clock")]
    Clock,
    #[token("%laneid")]
    LaneId,
    #[token("%pm0")]
    Pm0,
    #[token("%pm1")]
    Pm1,
    #[token("%pm2")]
    Pm2,
    #[token("%pm3")]
    Pm3,
    #[token("%pm4")]
    Pm4,
    #[token("%pm5")]
    Pm5,
    #[token("%pm6")]
    Pm6,
    #[token("%pm7")]
    Pm7,
    #[token("%clock64")]
    Clock64,
    #[token("%lanemask.eq")]
    LaneMaskEq,
    #[token("%lanemask.le")]
    LaneMaskLe,
    #[token("%lanemask.lt")]
    LaneMaskLt,
    #[token("%lanemask.ge")]
    LaneMaskGe,
    #[token("%lanemask.gt")]
    LaneMaskGt,
    #[token("%nctaid")]
    NCTAId,
    #[token("%smid")]
    SMId,
    #[token("%ctaid")]
    CTAId,
    #[token("%ctaid.x")]
    CTAIdX,
    #[token("%ctaid.y")]
    CTAIdY,
    #[token("%ctaid.z")]
    CTAIdZ,
    #[token("%ntid")]
    NTId,
    #[token("%ntid.x")]
    NTIdX,
    #[token("%ntid.y")]
    NTIdY,
    #[token("%ntid.z")]
    NTIdZ,
    #[token("%tid")]
    TId,
    #[token("%tid.x")]
    TIdX,
    #[token("%tid.y")]
    TIdY,
    #[token("%tid.z")]
    TIdZ,
    #[token("envreg<32>")]
    EnvReg32,
    #[token("nsmid")]
    NSMId,
    #[token("warpid")]
    WarpId,
    #[token("gridid")]
    GridId,
    #[token("nwarpid")]
    NWarpId,
    #[token("WARP_SZ")]
    WarpSize,
    // #[regex("[a-zA-Z_%$][a-zA-Z0-9_$]*", |lex| lex.slice().to_owned(), priority = 2)]
    #[regex(r"[a-zA-Z][a-zA-Z0-9_$]*|[_$%][a-zA-Z0-9_$]+", |lex| lex.slice().to_owned())]
    Identifier(String),
    #[token("@")]
    At,
    #[token("nop")]
    Nop,

    #[token(".f16")]
    F16,
    #[token(".f16x2")]
    F16x2,
    #[token(".f32")]
    F32,
    #[token(".tf32")]
    Tf32,
    #[token(".f64")]
    F64,
    #[token(".s8")]
    S8,
    #[token(".s16")]
    S16,
    #[token(".s32")]
    S32,
    #[token(".s64")]
    S64,
    #[token(".u8")]
    U8,
    #[token(".u16")]
    U16,
    #[token(".u32")]
    U32,
    #[token(".u64")]
    U64,
    #[token(".b8")]
    B8,
    #[token(".b16")]
    B16,
    #[token(".b32")]
    B32,
    #[token(".b64")]
    B64,
    #[token(".b128")]
    B128,
    #[token(".pred")]
    Pred,
    #[token(".bf16")]
    Bf16,

    /// binary literal:       0[bB]{bit}+U?
    #[regex("0[bB][01]+", |lex| lex.slice().parse::<u64>().unwrap(),priority = 2)]
    BinaryConstant(u64),
    /// decimal literal       {nonzero-digit}{digit}*U?
    #[regex("-?[1-9][0-9]*", |lex| lex.slice().parse::<i64>().unwrap(), priority = 2)]
    #[regex("-?0", |_| 0, priority = 2)]
    DecimalConstant(i64),
    /// hexadecimal literal:  0[xX]{hexdigit}+U?
    #[regex("0[xX][0-9a-fA-F]+", lex_hex, priority = 2)]
    HexConstant(u64),
    /// octal literal:        0{octal digit}+U?
    #[regex("0[0-7]+", lex_octal, priority = 2)]
    OctalConstant(u64),
    /// floating-point constants
    #[regex("0[dDfF][0-9a-fA-F]{8}", lex_single_float)]
    SingleConstant(f32),
    #[regex("0[dDfF][0-9a-fA-F]{16}", lex_double_float)]
    DoubleConstant(f64),

    // Operators and symbols
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token("%", priority = 10)]
    Modulo,
    #[token("=")]
    Assign,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("&&")]
    LogicalAnd,
    #[token("||")]
    LogicalOr,
    #[token("!")]
    LogicalNot,
    #[token("&")]
    BitwiseAnd,
    #[token("|")]
    BitwiseOr,
    #[token("^")]
    BitwiseXor,
    #[token("~")]
    BitwiseNot,
    #[token("<<")]
    ShiftLeft,
    #[token(">>")]
    ShiftRight,

    // Delimiters
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,

    // State Space
    /// Register, fast.
    #[token(".reg")]
    Reg,
    /// Special registers. Read-only; pre-defined; platform-specific.
    #[token(".sreg")]
    SReg,
    /// Shared, read-only memory.
    #[token(".const")]
    Const,
    /// Global memory, shared by all threads.
    #[token(".global")]
    Global,
    /// Local memory, private to each thread.
    #[token(".local")]
    Local,
    /// Kernel parameters, defined per-grid; or
    /// Function or local parameters, defined per-thread.
    #[token(".param")]
    Param,
    /// Addressable memory, defined per CTA, accessible to all threads in the cluster throughout the lifetime of the CTA that defines it.
    #[token(".shared")]
    Shared,
    /// Global texture memory (deprecated).
    #[token(".tex")]
    Tex,

    #[regex(r#""([^"\\\x00-\x1F]|\\(["\\bnfrt/]|u[a-fA-F0-9]{4}))*""#, |lex| lex.slice().to_owned())]
    String(String),
    // #[regex("//.*")]
    // Comment,
    #[token(".sync")]
    Sync,
    #[token(".async")]
    Async,

    #[token(".hi")]
    Hi,
    #[token(".lo")]
    Lo,
    #[token(".wide")]
    Wide,
    #[token(".eq")]
    Eq,
    #[token(".ne")]
    Ne,
    #[token(".lt")]
    Lt,
    #[token(".le")]
    Le,
    #[token(".gt")]
    Gt,
    #[token(".ge")]
    Ge,
    #[token(".equ")]
    Equ,
    #[token(".neu")]
    Neu,
    #[token(".ltu")]
    Ltu,
    #[token(".leu")]
    Leu,
    #[token(".gtu")]
    Gtu,
    #[token(".geu")]
    Geu,
    #[token(".ftz")]
    Ftz,
    #[token(".sat")]
    Saturate,
    #[token(".rn")]
    Rn,
    #[token(".rna")]
    Rna,
    #[token(".rz")]
    Rz,
    #[token(".rm")]
    Rm,
    #[token(".rp")]
    Rp,

    #[token(".commit_group")]
    CommitGroup,
    #[token(".cg")]
    Cg,
    #[token(".aligned")]
    Aligned,
    #[token(".wait_group")]
    WaitGroup,
    #[token(".m8n8")]
    M8N8,
    #[token(".m8n16")]
    M8N16,
    #[token(".x4")]
    X4,
    #[token(".x1")]
    X1,
    #[token(".x2")]
    X2,
    #[token(".row")]
    Row,
    #[token(".col")]
    Col,
    #[token(".m16n8k16")]
    M16N8K16,
    #[token(".m16n8k32")]
    M16N8K32,
    #[token(".m16n8k8")]
    M16N8K8,
    #[token(".bfly")]
    Bfly,
    #[token(".approx")]
    Approx,
    #[token(".trans")]
    Trans,
    #[token(".full")]
    Full,
    #[token(".to")]
    To,
    #[token(".uni")]
    Uni,
}

impl Token {
    pub fn is_function(&self) -> bool {
        matches!(self, Token::Func | Token::Entry)
    }
    pub fn is_directive(&self) -> bool {
        matches!(
            self,
            Token::AddressSize
                | Token::Trans
                | Token::ExplicitCluster
                | Token::MaxNReg
                | Token::Section
                | Token::Alias
                | Token::Extern
                | Token::MaxNTid
                | Token::Align(_)
                | Token::File
                | Token::MinNCTapersm
                | Token::BranchTargets
                | Token::Func
                | Token::NoReturn
                | Token::Target
                | Token::CallPrototype
                | Token::CallTargets
                | Token::Loc
                | Token::Pragma
                | Token::Version
                | Token::Common
                | Token::Visible
                | Token::MaxClusterRank
                | Token::ReqNCTaperCluster
                | Token::Weak
                | Token::Entry
                | Token::MaxNCTapersm
                | Token::ReqNTid
                | Token::Ptr
                | Token::Type
                | Token::Reg
                | Token::Param
                | Token::Async
                | Token::Sync
                | Token::Aligned
                | Token::CommitGroup
                | Token::Cg
                | Token::Shared
                | Token::Global
                | Token::WaitGroup
        )
    }
}

impl Eq for Token {}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::AddressSize => f.write_str(".address_size"),
            Token::ExplicitCluster => f.write_str(".explicitcluster"),
            Token::MaxNReg => f.write_str(".maxnreg"),
            Token::Section => f.write_str(".section"),
            Token::Alias => f.write_str(".alias"),
            Token::Extern => f.write_str(".extern"),
            Token::MaxNTid => f.write_str(".maxntid"),
            Token::Align(a) => f.write_fmt(format_args!(".align {}", a)),
            Token::File => f.write_str(".file"),
            Token::MinNCTapersm => f.write_str(".minnctapersm"),
            Token::BranchTargets => f.write_str(".branchtargets"),
            Token::Func => f.write_str(".func"),
            Token::NoReturn => f.write_str(".noreturn"),
            Token::Target => f.write_str(".target"),
            Token::CallPrototype => f.write_str(".callprototype"),
            Token::CallTargets => f.write_str(".calltargets"),
            Token::Loc => f.write_str(".loc"),
            Token::Pragma => f.write_str(".pragma"),
            Token::Version => f.write_str(".version"),
            Token::Common => f.write_str(".common"),
            Token::Visible => f.write_str(".visible"),
            Token::MaxClusterRank => f.write_str(".maxclusterrank"),
            Token::ReqNCTaperCluster => f.write_str(".reqnctapercluster"),
            Token::Weak => f.write_str(".weak"),
            Token::Entry => f.write_str(".entry"),
            Token::MaxNCTapersm => f.write_str(".maxnctapersm"),
            Token::ReqNTid => f.write_str(".reqntid"),
            Token::Ptr => f.write_str(".ptr"),
            Token::Type => f.write_str(".type"),
            Token::Abs => f.write_str("abs"),
            Token::Cvta => f.write_str("cvta"),
            Token::Membar => f.write_str("membar"),
            Token::Setp => f.write_str("setp"),
            Token::VAbsDiff4 => f.write_str("vabsdiff4"),
            Token::ActiveMask => f.write_str("activemask"),
            Token::Discard => f.write_str("discard"),
            Token::Min => f.write_str("min"),
            Token::Shf => f.write_str("shf"),
            Token::VAdd => f.write_str("vadd"),
            Token::Add => f.write_str("add"),
            Token::Div => f.write_str("div"),
            Token::Mma => f.write_str("mma"),
            Token::Shfl => f.write_str("shfl"),
            Token::VAdd2 => f.write_str("vadd2"),
            Token::AddC => f.write_str("addc"),
            Token::Dp2A => f.write_str("dp2a"),
            Token::Mov => f.write_str("mov"),
            Token::Shl => f.write_str("shl"),
            Token::VAdd4 => f.write_str("vadd4"),
            Token::Alloca => f.write_str("alloca"),
            Token::Dp4A => f.write_str("dp4a"),
            Token::MovMatrix => f.write_str("movmatrix"),
            Token::Shr => f.write_str("shr"),
            Token::VAverage2 => f.write_str("vavrg2"),
            Token::And => f.write_str("and"),
            Token::Elect => f.write_str("elect"),
            Token::Mul => f.write_str("mul"),
            Token::Sin => f.write_str("sin"),
            Token::VAverage4 => f.write_str("vavrg4"),
            Token::ApplyPriority => f.write_str("applypriority"),
            Token::Ex2 => f.write_str("ex2"),
            Token::Mul24 => f.write_str("mul24"),
            Token::Slct => f.write_str("slct"),
            Token::VMad => f.write_str("vmad"),
            Token::Atom => f.write_str("atom"),
            Token::Exit => f.write_str("exit"),
            Token::MultiMem => f.write_str("multimem"),
            Token::Sqrt => f.write_str("sqrt"),
            Token::VMax => f.write_str("vmax"),
            Token::Bar => f.write_str("bar"),
            Token::Fence => f.write_str("fence"),
            Token::NanoSleep => f.write_str("nanosleep"),
            Token::St => f.write_str("st"),
            Token::VMax2 => f.write_str("vmax2"),
            Token::Barrier => f.write_str("barrier"),
            Token::Fma => f.write_str("fma"),
            Token::Neg => f.write_str("neg"),
            Token::StackRestore => f.write_str("stackrestore"),
            Token::VMax4 => f.write_str("vmax4"),
            Token::Bfe => f.write_str("bfe"),
            Token::Fns => f.write_str("fns"),
            Token::Not => f.write_str("not"),
            Token::StackSave => f.write_str("stacksave"),
            Token::VMin => f.write_str("vmin"),
            Token::Bfi => f.write_str("bfi"),
            Token::GetCtaRank => f.write_str("getctarank"),
            Token::Or => f.write_str("or"),
            Token::StMatrix => f.write_str("stmatrix"),
            Token::VMin2 => f.write_str("vmin2"),
            Token::BFind => f.write_str("bfind"),
            Token::GridDepControl => f.write_str("griddepcontrol"),
            Token::PmEvent => f.write_str("pmevent"),
            Token::Sub => f.write_str("sub"),
            Token::VMin4 => f.write_str("vmin4"),
            Token::Bmsk => f.write_str("bmsk"),
            Token::IsSpaceP => f.write_str("isspacep"),
            Token::PopC => f.write_str("popc"),
            Token::SubC => f.write_str("subc"),
            Token::Vote => f.write_str("vote"),
            Token::Bra => f.write_str("bra"),
            Token::IsTypeP => f.write_str("istypep"),
            Token::Prefetch => f.write_str("prefetch"),
            Token::Suld => f.write_str("suld"),
            Token::VSet => f.write_str("vset"),
            Token::Brev => f.write_str("brev"),
            Token::Ld => f.write_str("ld"),
            Token::PrefetchU => f.write_str("prefetchu"),
            Token::Suq => f.write_str("suq"),
            Token::VSet2 => f.write_str("vset2"),
            Token::BrkPt => f.write_str("brkpt"),
            Token::LdMatrix => f.write_str("ldmatrix"),
            Token::Prmt => f.write_str("prmt"),
            Token::Sured => f.write_str("sured"),
            Token::VSet4 => f.write_str("vset4"),
            Token::Brx => f.write_str("brx"),
            Token::Ldu => f.write_str("ldu"),
            Token::Rcp => f.write_str("rcp"),
            Token::Sust => f.write_str("sust"),
            Token::VShl => f.write_str("vshl"),
            Token::Call => f.write_str("call"),
            Token::Lg2 => f.write_str("lg2"),
            Token::Red => f.write_str("red"),
            Token::SzExt => f.write_str("szext"),
            Token::VShr => f.write_str("vshr"),
            Token::Clz => f.write_str("clz"),
            Token::Lop3 => f.write_str("lop3"),
            Token::Redux => f.write_str("redux"),
            Token::Tanh => f.write_str("tanh"),
            Token::VSub => f.write_str("vsub"),
            Token::CNot => f.write_str("cnot"),
            Token::Mad => f.write_str("mad"),
            Token::Rem => f.write_str("rem"),
            Token::TestP => f.write_str("testp"),
            Token::VSub2 => f.write_str("vsub2"),
            Token::CopySign => f.write_str("copysign"),
            Token::Mad24 => f.write_str("mad24"),
            Token::Ret => f.write_str("ret"),
            Token::VSub4 => f.write_str("vsub4"),
            Token::Cos => f.write_str("cos"),
            Token::MadC => f.write_str("madc"),
            Token::RSqrt => f.write_str("rsqrt"),
            Token::Tld4 => f.write_str("tld4"),
            Token::WgMma => f.write_str("wgmma"),
            Token::ClusterLaunchControl => f.write_str("clusterlaunchcontrol"),
            Token::MapA => f.write_str("mapa"),
            Token::Sad => f.write_str("sad"),
            Token::Trap => f.write_str("trap"),
            Token::Wmma => f.write_str("wmma"),
            Token::Cp => f.write_str("cp"),
            Token::Match => f.write_str("match"),
            Token::SelP => f.write_str("selp"),
            Token::TxQ => f.write_str("txq"),
            Token::Xor => f.write_str("xor"),
            Token::CreatePolicy => f.write_str("createpolicy"),
            Token::Max => f.write_str("max"),
            Token::Set => f.write_str("set"),
            Token::VAbsDiff => f.write_str("vabsdiff"),
            Token::Cvt => f.write_str("cvt"),
            Token::MBarrier => f.write_str("mbarrier"),
            Token::SetMaxNReg => f.write_str("setmaxnreg"),
            Token::VAbsDiff2 => f.write_str("vabsdiff2"),
            Token::Clock => f.write_str("clock"),
            Token::LaneId => f.write_str("laneid"),
            Token::LaneMaskGt => f.write_str("lanemask.gt"),
            Token::Pm0 => f.write_str("pm0"),
            Token::Pm1 => f.write_str("pm1"),
            Token::Pm2 => f.write_str("pm2"),
            Token::Pm3 => f.write_str("pm3"),
            Token::Pm4 => f.write_str("pm4"),
            Token::Pm5 => f.write_str("pm5"),
            Token::Pm6 => f.write_str("pm6"),
            Token::Pm7 => f.write_str("pm7"),
            Token::Clock64 => f.write_str("clock64"),
            Token::LaneMaskEq => f.write_str("lanemask.eq"),
            Token::NCTAId => f.write_str("nctaid"),
            Token::SMId => f.write_str("smid"),
            Token::CTAId => f.write_str("ctaid"),
            Token::LaneMaskLe => f.write_str("lanemask.le"),
            Token::NTId => f.write_str("ntid"),
            Token::TId => f.write_str("tid"),
            Token::EnvReg32 => f.write_str("envreg<32>"),
            Token::LaneMaskLt => f.write_str("lanemask.lt"),
            Token::NSMId => f.write_str("nsmid"),
            Token::WarpId => f.write_str("warpid"),
            Token::GridId => f.write_str("gridid"),
            Token::LaneMaskGe => f.write_str("lanemask.ge"),
            Token::NWarpId => f.write_str("nwarpid"),
            Token::WarpSize => f.write_str("WARP_SZ"),
            Token::Identifier(ident) => write!(f, "{}", ident),
            Token::At => f.write_str("@"),
            Token::Nop => f.write_str("nop"),
            Token::F16 => f.write_str(".f16"),
            Token::F16x2 => f.write_str(".f16x2"),
            Token::F32 => f.write_str(".f32"),
            Token::F64 => f.write_str(".f64"),
            Token::S8 => f.write_str(".s8"),
            Token::S16 => f.write_str(".s16"),
            Token::S32 => f.write_str(".s32"),
            Token::S64 => f.write_str(".s64"),
            Token::U8 => f.write_str(".u8"),
            Token::U16 => f.write_str(".u16"),
            Token::U32 => f.write_str(".u32"),
            Token::U64 => f.write_str(".u64"),
            Token::B8 => f.write_str(".b8"),
            Token::B16 => f.write_str(".b16"),
            Token::B32 => f.write_str(".b32"),
            Token::B64 => f.write_str(".b64"),
            Token::B128 => f.write_str(".b128"),
            Token::Pred => f.write_str(".pred"),
            Token::Bf16 => f.write_str(".bf16"),
            Token::Tf32 => f.write_str(".tf32"),
            Token::BinaryConstant(num) => write!(f, "0b{}", num),
            Token::DecimalConstant(num) => write!(f, "{}", num),
            Token::HexConstant(num) => write!(f, "0x{:x}", num),
            Token::OctalConstant(num) => write!(f, "0o{:o}", num),
            Token::SingleConstant(num) => write!(f, "{}", num),
            Token::DoubleConstant(num) => write!(f, "{}", num),
            Token::Plus => f.write_str("+"),
            Token::Minus => f.write_str("-"),
            Token::Multiply => f.write_str("*"),
            Token::Divide => f.write_str("/"),
            Token::Modulo => f.write_str("%"),
            Token::Assign => f.write_str("="),
            Token::Equal => f.write_str("=="),
            Token::NotEqual => f.write_str("!="),
            Token::LessThan => f.write_str("<"),
            Token::GreaterThan => f.write_str(">"),
            Token::LessEqual => f.write_str("<="),
            Token::GreaterEqual => f.write_str(">="),
            Token::LogicalAnd => f.write_str("&&"),
            Token::LogicalOr => f.write_str("||"),
            Token::LogicalNot => f.write_str("!"),
            Token::BitwiseAnd => f.write_str("&"),
            Token::BitwiseOr => f.write_str("|"),
            Token::BitwiseXor => f.write_str("^"),
            Token::BitwiseNot => f.write_str("~"),
            Token::ShiftLeft => f.write_str("<<"),
            Token::ShiftRight => f.write_str(">>"),
            Token::LeftParen => f.write_str("("),
            Token::RightParen => f.write_str(")"),
            Token::LeftBrace => f.write_str("{"),
            Token::RightBrace => f.write_str("}"),
            Token::LeftBracket => f.write_str("["),
            Token::RightBracket => f.write_str("]"),
            Token::Comma => f.write_str(","),
            Token::Dot => f.write_str("."),
            Token::Semicolon => f.write_str(";"),
            Token::Colon => f.write_str(":"),
            Token::Reg => f.write_str(".reg"),
            Token::SReg => f.write_str(".sreg"),
            Token::Const => f.write_str(".const"),
            Token::Global => f.write_str(".global"),
            Token::Local => f.write_str(".local"),
            Token::Param => f.write_str(".param"),
            Token::Shared => f.write_str(".shared"),
            Token::Tex => f.write_str(".tex"),
            Token::String(s) => write!(f, "{}", s),
            Token::Sync => f.write_str(".sync"),
            Token::Async => f.write_str(".async"),
            Token::CTAIdX => f.write_str("%ctaid.x"),
            Token::CTAIdY => f.write_str("%ctaid.y"),
            Token::CTAIdZ => f.write_str("%ctaid.z"),
            Token::TIdX => f.write_str("%tid.x"),
            Token::TIdY => f.write_str("%tid.y"),
            Token::TIdZ => f.write_str("%tid.z"),
            Token::Hi => f.write_str(".hi"),
            Token::Lo => f.write_str(".lo"),
            Token::Wide => f.write_str(".wide"),
            Token::Eq => f.write_str(".eq"),
            Token::Ne => f.write_str(".ne"),
            Token::Lt => f.write_str(".lt"),
            Token::Le => f.write_str(".le"),
            Token::Ge => f.write_str(".ge"),
            Token::Gt => f.write_str(".gt"),
            Token::Ftz => f.write_str(".ftz"),
            Token::Saturate => f.write_str(".sat"),
            Token::Rn => f.write_str(".rn"),
            Token::Rz => f.write_str(".rz"),
            Token::Rm => f.write_str(".rm"),
            Token::Rp => f.write_str(".rp"),
            Token::Rna => f.write_str(".rna"),
            Token::CommitGroup => f.write_str(".commit_group"),
            Token::Cg => f.write_str(".cg"),
            Token::WaitGroup => f.write_str(".wait_group"),
            Token::Aligned => f.write_str(".aligned"),
            Token::M8N8 => f.write_str(".m8n8"),
            Token::M8N16 => f.write_str(".m8n16"),
            Token::X4 => f.write_str(".x4"),
            Token::X1 => f.write_str(".x1"),
            Token::X2 => f.write_str(".x2"),
            Token::Row => f.write_str(".row"),
            Token::Col => f.write_str(".col"),
            Token::M16N8K16 => f.write_str(".m16n8k16"),
            Token::M16N8K32 => f.write_str(".m16n8k32"),
            Token::Bfly => f.write_str(".bfly"),
            Token::Approx => f.write_str(".approx"),
            Token::Trans => f.write_str(".trans"),
            Token::Full => f.write_str(".full"),
            Token::NTIdX => f.write_str("%ntid.x"),
            Token::NTIdY => f.write_str("%ntid.y"),
            Token::NTIdZ => f.write_str("%ntid.z"),
            Token::To => f.write_str(".to"),
            Token::Uni => f.write_str(".uni"),
            Token::M16N8K8 => f.write_str(".m16n8k8"),
            Token::Equ => f.write_str(".equ"),
            Token::Neu => f.write_str(".neu"),
            Token::Ltu => f.write_str(".ltu"),
            Token::Leu => f.write_str(".leu"),
            Token::Gtu => f.write_str(".gtu"),
            Token::Geu => f.write_str(".geu"),
        }
    }
}
