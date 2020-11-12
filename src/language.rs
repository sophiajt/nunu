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

    /// Extends the current span to cover the range inclusive of both the old and given spans
    pub fn extend(&mut self, span: Span) {
        if span.start < self.start {
            self.start = span.start;
        }

        if span.end > self.end {
            self.end = span.end;
        }
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
    Call(Box<SpannedExpression>, Vec<SpannedExpression>),
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

pub struct LiteBlock {
    pub groups: Vec<LiteGroup>,
}
impl LiteBlock {
    pub fn new(groups: Vec<LiteGroup>) -> Self {
        Self { groups }
    }
}

/// A semicolon-seperated list of pipelines. Each pipeline, except the last, runs to completion and
/// does not automatically run `autoview` on the output. The last pipeline will automatically run `autoview` on its
/// output.
///
/// # Example
/// A group of two elements: a 3-step pipeline and a 1-step pipeline. Only `finally another thing` will automatically
/// have its contents viewed.
/// ```
/// > do this | do that | do one more; finally another thing
/// ```
#[derive(Debug)]
pub struct LiteGroup {
    pub pipelines: Vec<LitePipeline>,
}
impl LiteGroup {
    pub fn new() -> LiteGroup {
        LiteGroup { pipelines: vec![] }
    }
    pub fn is_empty(&self) -> bool {
        self.pipelines.is_empty()
    }
    pub fn push(&mut self, pipeline: LitePipeline) {
        self.pipelines.push(pipeline)
    }
}

/// A series of pipe-separated commands which together form a pipeline where data flows left-to-right
/// # Example
/// ```
/// > ls | where size > 10kb
/// ```
#[derive(Debug)]
pub struct LitePipeline {
    pub commands: Vec<LiteCommand>,
}
impl LitePipeline {
    pub fn new() -> LitePipeline {
        LitePipeline { commands: vec![] }
    }
    pub fn is_empty(&self) -> bool {
        self.commands.is_empty()
    }
    pub fn push(&mut self, command: LiteCommand) {
        self.commands.push(command)
    }
}

/// A single command with its associated arguments
/// # Example
/// ```
/// ls -la foo
/// ```
#[derive(Debug)]
pub struct LiteCommand {
    pub elements: Vec<Spanned<String>>,
}
impl LiteCommand {
    pub fn new() -> LiteCommand {
        LiteCommand { elements: vec![] }
    }
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }
    pub fn push(&mut self, element: Spanned<String>) {
        self.elements.push(element)
    }
}
