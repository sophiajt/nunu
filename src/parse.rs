use crate::language::{
    Expression, ExpressionGroup, ExpressionPipeline, ExpressionShape, LiteBlock, LiteCommand,
    LiteGroup, LitePipeline, ParseError, ParseSignature, Scope, Span, Spanned, SpannedItem, Token,
    TokenContents,
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
    shape: &ExpressionShape,
    scope: &Scope,
) -> (Spanned<Expression>, Option<ParseError>) {
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
        ExpressionShape::Any => {
            let shapes = vec![ExpressionShape::Integer, ExpressionShape::String];
            for shape in shapes.iter() {
                if let (s, None) = parse_expr(s, shape, scope) {
                    return (s, None);
                }
            }
            (
                garbage(s.span),
                Some(ParseError::UnexpectedType {
                    expected: "any shape".into(),
                    span: s.span,
                }),
            )
        }
    }
}

/// Easy shorthand function to create a garbage expression at the given span
pub fn garbage(span: Span) -> Spanned<Expression> {
    Expression::Garbage.spanned(span)
}

fn parse_external_call(
    call: LiteCommand,
    _scope: &Scope,
) -> (Spanned<Expression>, Option<ParseError>) {
    let head = call.elements[0].clone();
    let head_span = head.span;

    let args: Vec<Spanned<String>> = call.elements.into_iter().skip(1).collect();

    let end_span = if let Some(end) = args.last() {
        end.span
    } else {
        head.span
    };

    (
        Expression::ExternalCall(head, args).spanned(Span::new(head_span.start, end_span.end)),
        None,
    )
}

fn parse_internal_call(
    call: LiteCommand,
    signature: &ParseSignature,
    scope: &Scope,
) -> (Spanned<Expression>, Option<ParseError>) {
    let mut err = None;

    let (head, error) = parse_expr(&call.elements[0], &ExpressionShape::String, scope);
    let mut span = head.span;
    if err.is_none() {
        err = error;
    }

    let mut args = vec![];
    for arg in call.elements.iter().skip(1).zip(signature.iter()) {
        //TODO: this isn't actually how you parse arguments, just taking a shortcut for now
        let (arg, error) = parse_expr(arg.0, arg.1, scope);
        if err.is_none() {
            err = error;
        }
        span.extend(arg.span);
        args.push(arg);
    }

    (
        Expression::InternalCall(Box::new(head), args).spanned(span),
        err,
    )
}

fn parse_value_call(call: LiteCommand, scope: &Scope) -> (Spanned<Expression>, Option<ParseError>) {
    let mut err = None;

    let (head, error) = parse_expr(&call.elements[0], &ExpressionShape::String, scope);
    let mut span = head.span;
    if err.is_none() {
        err = error;
    }

    let mut args = vec![];
    for arg in call.elements.iter().skip(1) {
        let (arg, error) = parse_expr(arg, &ExpressionShape::Integer, scope);
        if err.is_none() {
            err = error;
        }
        span.extend(arg.span);
        args.push(arg);
    }

    (
        Expression::InternalCall(Box::new(head), args).spanned(span),
        err,
    )
}

fn parse_call(call: LiteCommand, scope: &Scope) -> (Spanned<Expression>, Option<ParseError>) {
    if call.elements.is_empty() {
        (
            Expression::Garbage.spanned_unknown(),
            Some(ParseError::UnexpectedType {
                expected: "call".into(),
                span: Span::unknown(),
            }),
        )
    } else if call.elements[0].item.starts_with('^') {
        parse_external_call(call, scope)
    } else if call.elements[0].item.starts_with('$') {
        parse_value_call(call, scope)
    } else if let Some(signature) = scope.get_signature(&call.elements[0].item) {
        parse_internal_call(call, &signature, scope)
    } else {
        parse_external_call(call, scope)
    }
}

fn parse_helper(
    lite_block: LiteBlock,
    scope: &Scope,
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
    scope: &Scope,
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
