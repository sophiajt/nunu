use num_bigint::BigInt;
use std::fmt::Debug;
use std::{collections::HashMap, ops::Deref};

#[derive(Debug, Clone)]
pub struct Spanned<T: Clone + Debug> {
    pub item: T,
    pub span: Span,
}

impl<T: Clone + Debug> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &<Self as std::ops::Deref>::Target {
        &self.item
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEof(String, Span),
    UnexpectedPipe(Span),
    UnexpectedType { expected: String, span: Span },
    DefinitionInPipeline(Span),
}

#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
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
pub trait SpannedItem: Sized + Debug + Clone {
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
impl<T: Debug + Clone> SpannedItem for T {}

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

#[derive(Debug, Clone)]
pub enum ExpressionShape {
    Integer,
    String,
    Block,
    Any,
}

#[derive(Clone, Debug)]
pub struct Scope<'a> {
    pub parent: Option<&'a Scope<'a>>,
    pub commands: HashMap<String, CommandDefinition>,
    pub aliases: HashMap<String, Vec<Spanned<String>>>,
}

impl<'a> Scope<'a> {
    pub fn new(parent: Option<&'a Scope>) -> Scope<'a> {
        Scope {
            parent,
            commands: HashMap::new(),
            aliases: HashMap::new(),
        }
    }
    pub fn get_signature(&self, name: &str) -> Option<ParseSignature> {
        if let Some(x) = self.commands.get(name) {
            Some(x.into())
        } else if let Some(parent) = &self.parent {
            parent.get_signature(name)
        } else {
            None
        }
    }
    pub fn get_alias(&self, name: &str) -> Option<Vec<Spanned<String>>> {
        if let Some(x) = self.aliases.get(name) {
            Some(x.clone())
        } else if let Some(parent) = &self.parent {
            parent.get_alias(name)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(BigInt),
    String(String),
    Variable(String),
    SetVariable(String, Box<Spanned<Expression>>),
    SetEnvVariable(String, Box<Spanned<Expression>>),
    InternalCall(Box<Spanned<Expression>>, Vec<Spanned<Expression>>),
    ExternalCall(Spanned<String>, Vec<Spanned<String>>),
    Block(Option<Vec<Parameter>>, ExpressionBlock),
    Noop,
    Garbage,
}

#[derive(Debug, Clone)]
pub struct ExpressionPipeline {
    pub pipeline: Vec<Spanned<Expression>>,
}
impl ExpressionPipeline {
    pub fn new() -> ExpressionPipeline {
        Self { pipeline: vec![] }
    }
    pub fn push(&mut self, expression: Spanned<Expression>) {
        self.pipeline.push(expression)
    }
}

impl Default for ExpressionPipeline {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
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

impl Default for ExpressionGroup {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionBlock {
    pub groups: Vec<ExpressionGroup>,
    pub definitions: HashMap<String, CommandDefinition>,
}

impl ExpressionBlock {
    pub fn new() -> ExpressionBlock {
        Self {
            groups: vec![],
            definitions: HashMap::new(),
        }
    }
    pub fn push(&mut self, pipeline: ExpressionGroup) {
        self.groups.push(pipeline)
    }
}

impl Default for ExpressionBlock {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct Parameter {
    name: String,
    shape: ExpressionShape,
}
impl Parameter {
    pub fn new(name: String, shape: ExpressionShape) -> Self {
        Self { name, shape }
    }
}
#[derive(Debug, Clone)]
pub struct CommandDefinition {
    pub params: Vec<Parameter>,
    pub block: Option<ExpressionBlock>,
}

impl Into<ParseSignature> for CommandDefinition {
    fn into(self) -> ParseSignature {
        let mut output = vec![];
        for param in self.params {
            output.push(param.shape);
        }

        output
    }
}

impl Into<ParseSignature> for &CommandDefinition {
    fn into(self) -> ParseSignature {
        let mut output = vec![];
        for param in &self.params {
            output.push(param.shape.clone());
        }

        output
    }
}

impl CommandDefinition {
    pub fn new(params: Vec<Parameter>, block: Option<ExpressionBlock>) -> Self {
        Self { params, block }
    }
}

pub struct LiteBlock {
    pub groups: Vec<LiteGroup>,
}
impl LiteBlock {
    pub fn new(groups: Vec<LiteGroup>) -> Self {
        Self { groups }
    }
    pub fn head(&self) -> Option<Spanned<String>> {
        if let Some(group) = self.groups.get(0) {
            if let Some(pipeline) = group.pipelines.get(0) {
                if let Some(command) = pipeline.commands.get(0) {
                    if let Some(head) = command.elements.get(0) {
                        return Some(head.clone());
                    }
                }
            }
        }
        None
    }
    pub fn remove_head(&mut self) {
        if let Some(group) = self.groups.get_mut(0) {
            if let Some(pipeline) = group.pipelines.get_mut(0) {
                if let Some(command) = pipeline.commands.get_mut(0) {
                    if !command.elements.is_empty() {
                        command.elements.remove(0);
                    }
                }
            }
        }
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
