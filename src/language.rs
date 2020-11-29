use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::{collections::HashMap, ops::Deref};

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
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
    UnexpectedSemicolon(Span),
    UnexpectedType { expected: String, span: Span },
    DefinitionInPipeline(Span),
    MissingMandatoryPositional(String, Span),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
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

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
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

#[derive(Debug, Clone)]
pub struct Token {
    pub contents: TokenContents,
    pub span: Span,
}
impl Token {
    pub fn new(contents: TokenContents, span: Span) -> Token {
        Token { contents, span }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
pub enum ExpressionShape {
    Integer,
    Decimal,
    Number,
    String,
    Block,
    Table,
    List,
    TypedNumber,
    FullColumnPath,
    Operator,
    Math,
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

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
pub enum Number {
    Int(BigInt),
    Decimal(BigDecimal),
}

impl Into<Number> for u64 {
    fn into(self) -> Number {
        Number::Int(BigInt::from(self))
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
pub enum Expression {
    Integer(BigInt),
    Decimal(BigDecimal),
    TypedNumber(Spanned<Number>, Spanned<Unit>),
    String(String),
    Variable(String),
    SetVariable(String, Box<Spanned<Expression>>),
    SetEnvVariable(String, Box<Spanned<Expression>>),
    InternalCall(Box<Spanned<Expression>>, Vec<Spanned<Expression>>),
    ExternalCall(Spanned<String>, Vec<Spanned<Expression>>),
    List(Vec<Spanned<Expression>>),
    Table(Vec<Spanned<Expression>>, Vec<Vec<Spanned<Expression>>>),
    Block(Option<Vec<Parameter>>, ExpressionBlock),
    Invocation(ExpressionBlock),
    ColumnPath(Box<ColumnPath>),
    Operator(Operator),
    Binary(Box<Binary>),
    Garbage,
}

impl Expression {
    pub fn precedence(&self) -> usize {
        match self {
            Expression::Operator(operator) => {
                // Higher precedence binds tighter

                match operator {
                    Operator::Multiply | Operator::Divide | Operator::Modulo => 100,
                    Operator::Plus | Operator::Minus => 90,
                    Operator::NotContains
                    | Operator::Contains
                    | Operator::LessThan
                    | Operator::LessThanOrEqual
                    | Operator::GreaterThan
                    | Operator::GreaterThanOrEqual
                    | Operator::Equal
                    | Operator::NotEqual
                    | Operator::In
                    | Operator::NotIn => 80,
                    Operator::And => 50,
                    Operator::Or => 40, // TODO: should we have And and Or be different precedence?
                }
            }
            _ => 0,
        }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
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

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ExpressionBlock {
    pub groups: Vec<ExpressionGroup>,
    pub definitions: HashMap<String, CommandDefinition>,
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for ExpressionBlock {
    /// Create the hash function to allow the Hash trait for dictionaries
    fn hash<H: Hasher>(&self, state: &mut H) {
        let entries = self.definitions.clone();
        let mut keys = entries.keys().collect::<Vec<&String>>();
        keys.sort();
        keys.hash(state);
        entries
            .values()
            .collect::<Vec<&CommandDefinition>>()
            .hash(state);
    }
}

impl PartialOrd for ExpressionBlock {
    /// Compare two dictionaries for sort ordering
    fn partial_cmp(&self, other: &ExpressionBlock) -> Option<Ordering> {
        let this: Vec<&String> = self.definitions.keys().collect();
        let that: Vec<&String> = other.definitions.keys().collect();

        if this != that {
            return this.partial_cmp(&that);
        }

        let this: Vec<&CommandDefinition> = self.definitions.values().collect();
        let that: Vec<&CommandDefinition> = other.definitions.values().collect();

        this.partial_cmp(&that)
    }
}

impl Ord for ExpressionBlock {
    /// Compare two dictionaries for ordering
    fn cmp(&self, other: &ExpressionBlock) -> Ordering {
        let this: Vec<&String> = self.definitions.keys().collect();
        let that: Vec<&String> = other.definitions.keys().collect();

        if this != that {
            return this.cmp(&that);
        }

        let this: Vec<&CommandDefinition> = self.definitions.values().collect();
        let that: Vec<&CommandDefinition> = other.definitions.values().collect();

        this.cmp(&that)
    }
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

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
pub struct Parameter {
    name: String,
    shape: ExpressionShape,
}
impl Parameter {
    pub fn new(name: String, shape: ExpressionShape) -> Self {
        Self { name, shape }
    }
}
#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
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

#[derive(Debug)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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

/// A PathMember that has yet to be spanned so that it can be used in later processing
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ColumnPathMember {
    String(String),
    Int(BigInt),
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
pub struct ColumnPath {
    pub head: Spanned<Expression>,
    pub tail: Vec<Spanned<ColumnPathMember>>,
}

impl ColumnPath {
    pub fn new(head: Spanned<Expression>, tail: Vec<Spanned<ColumnPathMember>>) -> Self {
        Self { head, tail }
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash, Copy)]
pub enum Unit {
    // Filesize units
    Byte,
    Kilobyte,
    Megabyte,
    Gigabyte,
    Terabyte,
    Petabyte,

    // Duration units
    Nanosecond,
    Microsecond,
    Millisecond,
    Second,
    Minute,
    Hour,
    Day,
    Week,
    Month,
    Year,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, Eq, Hash, PartialEq)]
pub enum Operator {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Contains,
    NotContains,
    Plus,
    Minus,
    Multiply,
    Divide,
    In,
    NotIn,
    Modulo,
    And,
    Or,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
pub struct Binary {
    pub left: Spanned<Expression>,
    pub op: Spanned<Expression>,
    pub right: Spanned<Expression>,
}
