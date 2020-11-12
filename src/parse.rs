use crate::language::{
    Expression, ExpressionGroup, ExpressionPipeline, ExpressionShape, LiteBlock, LiteCommand,
    LiteGroup, LitePipeline, ParseError, Scope, Span, Spanned, SpannedExpression, SpannedItem,
    Token, TokenContents,
};
use crate::lite_parse::lex;

fn group(tokens: Vec<Token>) -> (LiteBlock, Option<ParseError>) {
    let mut groups = vec![];
    let mut group = LiteGroup::new();
    let mut pipeline = LitePipeline::new();
    let mut command = LiteCommand::new();

    for token in tokens {
        match token.contents {
            TokenContents::EOL => {
                if !command.is_empty() {
                    pipeline.push(command);
                    command = LiteCommand::new();
                }
                if !pipeline.is_empty() {
                    group.push(pipeline);
                    pipeline = LitePipeline::new();
                }
                if !group.is_empty() {
                    groups.push(group);
                    group = LiteGroup::new();
                }
            }
            TokenContents::Pipe => {
                if !command.is_empty() {
                    pipeline.push(command);
                    command = LiteCommand::new();
                } else {
                    return (
                        LiteBlock::new(groups),
                        Some(ParseError::UnexpectedPipe(token.span)),
                    );
                }
            }
            TokenContents::Semicolon => {
                if !command.is_empty() {
                    pipeline.push(command);
                    command = LiteCommand::new();
                }
                if !pipeline.is_empty() {
                    group.push(pipeline);
                    pipeline = LitePipeline::new();
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
        groups.push(group);
    }

    (LiteBlock::new(groups), None)
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

fn parse_call(call: LiteCommand, _scope: &Box<Scope>) -> (SpannedExpression, Option<ParseError>) {
    if call.elements.is_empty() {
        (
            Expression::Garbage.spanned_unknown(),
            Some(ParseError::UnexpectedType {
                expected: "call".into(),
                span: Span::unknown(),
            }),
        )
    } else {
        let mut err = None;
        let (head, error) = parse_expr(&call.elements[0], ExpressionShape::String);
        let mut span = head.span;
        if err.is_none() {
            err = error;
        }

        let mut args = vec![];
        for arg in call.elements.iter().skip(1) {
            let (arg, error) = parse_expr(arg, ExpressionShape::Integer);
            if err.is_none() {
                err = error;
            }
            span.extend(arg.span);
            args.push(arg);
        }

        (Expression::Call(Box::new(head), args).spanned(span), err)
    }
}

fn parse_helper(
    lite_block: LiteBlock,
    scope: &Box<Scope>,
) -> (Vec<ExpressionGroup>, Option<ParseError>) {
    let mut output = vec![];
    let mut err = None;

    for group in lite_block.groups {
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
