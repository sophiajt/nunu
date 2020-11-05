use crate::language::{
    Expression, ExpressionGroup, ExpressionPipeline, ExpressionShape, ParseError, Scope, Span,
    Spanned, SpannedExpression, SpannedItem, Token, TokenContents,
};
use crate::lite_parse::lex;
use std::fmt::Debug;

#[derive(Debug)]
pub struct Group {
    pub pipelines: Vec<Pipeline>,
}
impl Group {
    pub fn new() -> Group {
        Group { pipelines: vec![] }
    }
    pub fn is_empty(&self) -> bool {
        self.pipelines.is_empty()
    }
    pub fn push(&mut self, pipeline: Pipeline) {
        self.pipelines.push(pipeline)
    }
}

#[derive(Debug)]
pub struct Pipeline {
    pub commands: Vec<Command>,
}
impl Pipeline {
    pub fn new() -> Pipeline {
        Pipeline { commands: vec![] }
    }
    pub fn is_empty(&self) -> bool {
        self.commands.is_empty()
    }
    pub fn push(&mut self, command: Command) {
        self.commands.push(command)
    }
}

#[derive(Debug)]
pub struct Command {
    pub elements: Vec<Spanned<String>>,
}
impl Command {
    pub fn new() -> Command {
        Command { elements: vec![] }
    }
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }
    pub fn push(&mut self, element: Spanned<String>) {
        self.elements.push(element)
    }
}

fn group(tokens: Vec<Token>) -> (Vec<Group>, Option<ParseError>) {
    let mut output = vec![];
    let mut group = Group::new();
    let mut pipeline = Pipeline::new();
    let mut command = Command::new();

    for token in tokens {
        match token.contents {
            TokenContents::EOL => {
                if !command.is_empty() {
                    pipeline.push(command);
                    command = Command::new();
                }
                if !pipeline.is_empty() {
                    group.push(pipeline);
                    pipeline = Pipeline::new();
                }
                if !group.is_empty() {
                    output.push(group);
                    group = Group::new();
                }
            }
            TokenContents::Pipe => {
                if !command.is_empty() {
                    pipeline.push(command);
                    command = Command::new();
                } else {
                    return (output, Some(ParseError::UnexpectedPipe(token.span)));
                }
            }
            TokenContents::Semicolon => {
                if !command.is_empty() {
                    pipeline.push(command);
                    command = Command::new();
                }
                if !pipeline.is_empty() {
                    group.push(pipeline);
                    pipeline = Pipeline::new();
                }
            }
            TokenContents::Bare(bare) => {
                command.push(bare.spanned(token.span));
            }
        }
    }
    if !command.is_empty() {
        pipeline.push(command);
    }
    if !pipeline.is_empty() {
        group.push(pipeline);
    }
    if !group.is_empty() {
        output.push(group);
    }

    (output, None)
}

fn parse_expr(
    s: &Spanned<String>,
    shape: ExpressionShape,
) -> (SpannedExpression, Option<ParseError>) {
    match shape {
        ExpressionShape::Integer => {
            if let Some(i) = num_bigint::BigInt::parse_bytes(s.item.as_bytes(), 10) {
                (Expression::Integer(i).spanned(s.span), None)
            } else {
                (
                    Expression::Garbage.spanned(s.span),
                    Some(ParseError::UnexpectedType {
                        expected: "Integer".into(),
                        span: s.span,
                    }),
                )
            }
        }
        ExpressionShape::String => {
            // Pretty much everything else counts as some kind of string
            (Expression::String(s.item.clone()).spanned(s.span), None)
        }
    }
}

fn parse_call(call: Command, _scope: &Box<Scope>) -> (SpannedExpression, Option<ParseError>) {
    if call.elements.is_empty() {
        (
            Expression::Garbage.spanned_unknown(),
            Some(ParseError::UnexpectedType {
                expected: "call".into(),
                span: Span::unknown(),
            }),
        )
    } else {
        parse_expr(&call.elements[0], ExpressionShape::String)
    }
}

fn parse_helper(
    groups: Vec<Group>,
    scope: &Box<Scope>,
) -> (Vec<ExpressionGroup>, Option<ParseError>) {
    let mut output = vec![];
    let mut err = None;

    for group in groups {
        let mut out_group = ExpressionGroup::new();
        for pipeline in group.pipelines {
            let mut out_pipe = ExpressionPipeline::new();
            for call in pipeline.commands {
                let (parsed, error) = parse_call(call, scope);
                if err.is_none() {
                    err = error;
                }
                out_pipe.push(parsed);
            }
            out_group.push(out_pipe);
        }
        output.push(out_group);
    }

    (output, err)
}

pub fn parse(
    input: &str,
    span_offset: usize,
    scope: &Box<Scope>,
) -> (Vec<ExpressionGroup>, Option<ParseError>) {
    let (output, error) = lex(input, span_offset);
    if error.is_some() {
        return (vec![], error);
    }
    let (groups, error) = group(output);
    if error.is_some() {
        return (vec![], error);
    }

    parse_helper(groups, scope)
}
