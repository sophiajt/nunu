use num_bigint::BigInt;
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Debug)]
pub struct Spanned<T: Debug> {
    pub item: T,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEof(String, Span),
    UnexpectedPipe(Span),
    UnexpectedType { expected: String, span: Span },
}

#[derive(Copy, Clone, Debug)]
pub struct Span {
    start: usize,
    end: usize,
}
impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
    pub fn unknown() -> Span {
        Span { start: 0, end: 0 }
    }
}
pub trait SpannedItem: Sized + Debug {
    /// Converts a value into a Spanned value
    fn spanned(self, span: impl Into<Span>) -> Spanned<Self> {
        Spanned {
            item: self,
            span: span.into(),
        }
    }

    /// Converts a value into a Spanned value, using an unknown Span
    fn spanned_unknown(self) -> Spanned<Self> {
        Spanned {
            item: self,
            span: Span::unknown(),
        }
    }
}
impl<T: Debug> SpannedItem for T {}

#[derive(Debug)]
pub struct Token {
    pub contents: TokenContents,
    pub span: Span,
}
impl Token {
    pub fn new(contents: TokenContents, span: Span) -> Token {
        Token { contents, span }
    }
}

#[derive(Debug)]
pub enum TokenContents {
    Bare(String),
    Pipe,
    Semicolon,
    EOL,
}

#[derive(Clone, Copy)]
pub enum BlockKind {
    Paren,
    CurlyBracket,
    SquareBracket,
}
impl From<BlockKind> for char {
    fn from(bk: BlockKind) -> char {
        match bk {
            BlockKind::Paren => ')',
            BlockKind::SquareBracket => ']',
            BlockKind::CurlyBracket => '}',
        }
    }
}

pub type ParseSignature = Vec<ExpressionShape>;

pub enum ExpressionShape {
    Integer,
    String,
}

pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub commands: HashMap<String, ParseSignature>,
}

#[derive(Debug)]
pub enum Expression {
    Integer(BigInt),
    String(String),
    Garbage,
}

pub type SpannedExpression = Spanned<Expression>;

#[derive(Debug)]
pub struct ExpressionPipeline {
    pub pipeline: Vec<SpannedExpression>,
}
impl ExpressionPipeline {
    pub fn new() -> ExpressionPipeline {
        Self { pipeline: vec![] }
    }
    pub fn push(&mut self, expression: SpannedExpression) {
        self.pipeline.push(expression)
    }
}

#[derive(Debug)]
pub struct ExpressionGroup {
    pub pipelines: Vec<ExpressionPipeline>,
}
impl ExpressionGroup {
    pub fn new() -> ExpressionGroup {
        Self { pipelines: vec![] }
    }
    pub fn push(&mut self, pipeline: ExpressionPipeline) {
        self.pipelines.push(pipeline)
    }
}
