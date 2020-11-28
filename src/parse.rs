use crate::language::{
    ColumnPath, ColumnPathMember, CommandDefinition, Expression, ExpressionBlock, ExpressionGroup,
    ExpressionPipeline, ExpressionShape, LiteBlock, LiteCommand, LiteGroup, LitePipeline, Number,
    Parameter, ParseError, Scope, Span, Spanned, SpannedItem, Token, TokenContents, Unit,
};
use crate::lex::lex;
use num_bigint::BigInt;

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

fn parse_list(
    s: &Spanned<String>,
    scope: &Scope,
) -> (Vec<Spanned<Expression>>, Option<ParseError>) {
    let contents = trim_square_braces(&s);

    let (output, error) = lex(&contents.item, contents.span.start);
    if error.is_some() {
        return (vec![], error);
    }

    let (lite_block, error) = group(output);
    if error.is_some() {
        return (vec![], error);
    }

    if lite_block.groups.is_empty() {
        return (vec![], None);
    }
    let lite_pipeline = &lite_block.groups[0];
    let mut output = vec![];
    let mut error = None;
    for lite_pipeline in &lite_pipeline.pipelines {
        for lite_inner in &lite_pipeline.commands {
            for part in &lite_inner.elements {
                let item = if part.ends_with(',') {
                    let mut str: String = part.item.clone();
                    str.pop();
                    str.spanned(Span::new(part.span.start, part.span.end - 1))
                } else {
                    part.clone()
                };
                let (part, err) = parse_expr(&item, &ExpressionShape::Any, scope);
                output.push(part);

                if error.is_none() {
                    error = err;
                }
            }
        }
    }

    (output, error)
}

fn parse_invocation(
    lite_arg: &Spanned<String>,
    scope: &Scope,
) -> (Spanned<Expression>, Option<ParseError>) {
    // We have a command invocation
    let string: String = lite_arg
        .item
        .chars()
        .skip(2)
        .take(lite_arg.item.len() - 3)
        .collect();

    let (parsed_block, err) = parse_block(
        &string.spanned(Span::new(lite_arg.span.start + 2, lite_arg.span.end - 1)),
        false,
        scope,
    );

    match parsed_block.item {
        Expression::Block(_, block) => (Expression::Invocation(block).spanned(lite_arg.span), err),
        _ => (garbage(lite_arg.span), err),
    }
}

/// Parses a column path, adding in the preceding reference to $it if it's elided
pub fn parse_full_column_path(
    lite_arg: &Spanned<String>,
    scope: &Scope,
) -> (Spanned<Expression>, Option<ParseError>) {
    let mut delimiter = '.';
    let mut inside_delimiter = false;
    let mut output = vec![];
    let mut current_part = String::new();
    let mut start_index = 0;
    let mut last_index = 0;
    let mut error = None;

    let mut head = None;

    for (idx, c) in lite_arg.item.char_indices() {
        last_index = idx;
        if inside_delimiter {
            if c == delimiter {
                inside_delimiter = false;
            }
        } else if c == '(' {
            inside_delimiter = true;
            delimiter = ')';
        } else if c == '\'' || c == '"' {
            inside_delimiter = true;
            delimiter = c;
        } else if c == '.' {
            let part_span = Span::new(
                lite_arg.span.start() + start_index,
                lite_arg.span.start() + idx,
            );

            if head.is_none() && current_part.starts_with("$(") && current_part.ends_with(')') {
                let (invoc, err) =
                    parse_invocation(&current_part.clone().spanned(part_span), scope);
                if error.is_none() {
                    error = err;
                }
                head = Some(invoc);
            } else if head.is_none() && current_part.starts_with('$') {
                // We have the variable head
                head = Some(Expression::Variable(current_part.clone()).spanned(part_span))
            } else if let Ok(row_number) = current_part.parse::<u64>() {
                output.push(ColumnPathMember::Int(BigInt::from(row_number)).spanned(part_span));
            } else {
                let current_part = trim_quotes(&current_part);
                output.push(ColumnPathMember::String(current_part.clone()).spanned(part_span));
            }
            current_part.clear();
            // Note: I believe this is safe because of the delimiter we're using, but if we get fancy with
            // unicode we'll need to change this
            start_index = idx + '.'.len_utf8();
            continue;
        }
        current_part.push(c);
    }

    if !current_part.is_empty() {
        let part_span = Span::new(
            lite_arg.span.start() + start_index,
            lite_arg.span.start() + last_index + 1,
        );

        if head.is_none() {
            if current_part.starts_with("$(") && current_part.ends_with(')') {
                let (invoc, err) = parse_invocation(&current_part.spanned(part_span), scope);
                if error.is_none() {
                    error = err;
                }
                head = Some(invoc);
            } else if current_part.starts_with('$') {
                head = Some(Expression::Variable(current_part).spanned(lite_arg.span));
            } else if let Ok(row_number) = current_part.parse::<u64>() {
                output.push(ColumnPathMember::Int(BigInt::from(row_number)).spanned(part_span));
            } else {
                let current_part = trim_quotes(&current_part);
                output.push(ColumnPathMember::String(current_part).spanned(part_span));
            }
        } else if let Ok(row_number) = current_part.parse::<u64>() {
            output.push(ColumnPathMember::Int(BigInt::from(row_number)).spanned(part_span));
        } else {
            let current_part = trim_quotes(&current_part);
            output.push(ColumnPathMember::String(current_part).spanned(part_span));
        }
    }

    if let Some(head) = head {
        let column_path = ColumnPath::new(head, output);

        (
            Expression::ColumnPath(Box::new(column_path)).spanned(lite_arg.span),
            error,
        )
    } else {
        let column_path = ColumnPath::new(
            Expression::Variable("$it".into()).spanned(lite_arg.span),
            output,
        );
        (
            Expression::ColumnPath(Box::new(column_path)).spanned(lite_arg.span),
            error,
        )
    }
}

fn parse_block(
    s: &Spanned<String>,
    maybe_params: bool,
    scope: &Scope,
) -> (Spanned<Expression>, Option<ParseError>) {
    let contents = trim_curly_braces(&s);
    let mut error = None;
    let (output, err) = lex(&contents.item, contents.span.start);
    if err.is_some() {
        return (garbage(s.span), err);
    }

    let (mut lite_block, err) = group(output);
    if err.is_some() {
        return (garbage(s.span), err);
    }

    let params = if maybe_params {
        // Check for a parameter list
        if let Some(head) = lite_block.head() {
            if head.starts_with('[') {
                let (params, err) = parse_parameters(&head, scope);
                if error.is_none() {
                    error = err;
                }
                lite_block.remove_head();
                Some(params)
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    };

    let mut scope = Scope::new(Some(scope));

    let (result, err) = parse_helper(lite_block, &mut scope);
    if error.is_none() {
        error = err;
    }
    (Expression::Block(params, result).spanned(s.span), error)
}

fn parse_table(s: &Spanned<String>, scope: &Scope) -> (Spanned<Expression>, Option<ParseError>) {
    let contents = trim_square_braces(&s);
    let mut error = None;
    let (output, err) = lex(&contents.item, contents.span.start);
    if err.is_some() {
        return (garbage(s.span), err);
    }

    let (lite_block, err) = group(output);
    if err.is_some() {
        return (garbage(s.span), err);
    }

    let mut output = vec![];

    if lite_block.groups.len() != 1
        || lite_block.groups[0].pipelines.len() != 2
        || lite_block.groups[0].pipelines[0].commands.len() != 1
        || lite_block.groups[0].pipelines[1].commands.len() != 1
        || lite_block.groups[0].pipelines[0].commands[0].elements.len() != 1
        || lite_block.groups[0].pipelines[1].commands[0].elements.len() != 1
    {
        return (
            garbage(s.span),
            Some(ParseError::UnexpectedType {
                expected: "table".into(),
                span: s.span,
            }),
        );
    }

    // Header

    let head = &lite_block.groups[0].pipelines[0].commands[0].elements[0];

    let (headers, err) = parse_list(&head, scope);
    if error.is_none() {
        error = err;
    }

    // Cells
    let lite_cells = &lite_block.groups[0].pipelines[1].commands[0];

    for arg in &lite_cells.elements {
        let (inner_cell, err) = parse_list(arg, scope);
        if error.is_none() {
            error = err;
        }
        output.push(inner_cell);
    }

    (Expression::Table(headers, output).spanned(s.span), error)
}

/// Parse a typed number, eg '10kb'
fn parse_typed_number(lite_arg: &Spanned<String>) -> (Spanned<Expression>, Option<ParseError>) {
    let unit_groups = [
        (Unit::Byte, vec!["b", "B"]),
        (Unit::Kilobyte, vec!["kb", "KB", "Kb", "kB"]),
        (Unit::Megabyte, vec!["mb", "MB", "Mb", "mB"]),
        (Unit::Gigabyte, vec!["gb", "GB", "Gb", "gB"]),
        (Unit::Terabyte, vec!["tb", "TB", "Tb", "tB"]),
        (Unit::Petabyte, vec!["pb", "PB", "Pb", "pB"]),
        (Unit::Nanosecond, vec!["ns"]),
        (Unit::Microsecond, vec!["us"]),
        (Unit::Millisecond, vec!["ms"]),
        (Unit::Second, vec!["sec"]),
        (Unit::Minute, vec!["min"]),
        (Unit::Hour, vec!["hr"]),
        (Unit::Day, vec!["day"]),
        (Unit::Week, vec!["wk"]),
        (Unit::Month, vec!["mon"]),
        (Unit::Year, vec!["yr"]),
    ];

    for unit_group in unit_groups.iter() {
        for unit in unit_group.1.iter() {
            if lite_arg.item.ends_with(unit) {
                let mut lhs = lite_arg.item.clone();

                for _ in 0..unit.len() {
                    lhs.pop();
                }

                // these units are allowed to be signed
                if let Ok(x) = lhs.parse::<BigInt>() {
                    let x = Number::Int(x);
                    let lhs_span =
                        Span::new(lite_arg.span.start(), lite_arg.span.start() + lhs.len());
                    let unit_span =
                        Span::new(lite_arg.span.start() + lhs.len(), lite_arg.span.end());
                    return (
                        Expression::TypedNumber(
                            x.spanned(lhs_span),
                            unit_group.0.spanned(unit_span),
                        )
                        .spanned(lite_arg.span),
                        None,
                    );
                }
            }
        }
    }

    (
        garbage(lite_arg.span),
        Some(ParseError::UnexpectedType {
            expected: "unit".into(),
            span: lite_arg.span,
        }),
    )
}

fn parse_expr(
    s: &Spanned<String>,
    shape: &ExpressionShape,
    scope: &Scope,
) -> (Spanned<Expression>, Option<ParseError>) {
    // TODO: pass variable paths and invocations
    if s.starts_with('$') {
        return parse_full_column_path(s, scope);
    }

    match shape {
        ExpressionShape::Integer => {
            if let Some(i) = num_bigint::BigInt::parse_bytes(s.as_bytes(), 10) {
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
        ExpressionShape::Decimal => {
            if let Some(i) = bigdecimal::BigDecimal::parse_bytes(s.as_bytes(), 10) {
                (Expression::Decimal(i).spanned(s.span), None)
            } else {
                (
                    Expression::Garbage.spanned(s.span),
                    Some(ParseError::UnexpectedType {
                        expected: "Decimal".into(),
                        span: s.span,
                    }),
                )
            }
        }
        ExpressionShape::Number => {
            let shapes = vec![ExpressionShape::Integer, ExpressionShape::Decimal];
            for shape in shapes.iter() {
                if let (s, None) = parse_expr(s, shape, scope) {
                    return (s, None);
                }
            }
            (
                garbage(s.span),
                Some(ParseError::UnexpectedType {
                    expected: "number".into(),
                    span: s.span,
                }),
            )
        }
        ExpressionShape::TypedNumber => parse_typed_number(s),
        ExpressionShape::String => {
            // Pretty much everything else counts as some kind of string
            (Expression::String(s.item.clone()).spanned(s.span), None)
        }
        ExpressionShape::Block => {
            if s.starts_with('{') && s.ends_with('}') {
                parse_block(s, true, scope)
            } else {
                (
                    garbage(s.span),
                    Some(ParseError::UnexpectedType {
                        expected: "Block".into(),
                        span: s.span,
                    }),
                )
            }
        }
        ExpressionShape::Table => {
            if s.starts_with('[') && s.ends_with(']') {
                parse_table(s, scope)
            } else {
                (
                    garbage(s.span),
                    Some(ParseError::UnexpectedType {
                        expected: "Table".into(),
                        span: s.span,
                    }),
                )
            }
        }
        ExpressionShape::List => {
            if s.starts_with('[') && s.ends_with(']') {
                let (result, err) = parse_list(s, scope);
                (Expression::List(result).spanned(s.span), err)
            } else {
                (
                    garbage(s.span),
                    Some(ParseError::UnexpectedType {
                        expected: "List".into(),
                        span: s.span,
                    }),
                )
            }
        }
        ExpressionShape::Any => {
            let shapes = vec![
                ExpressionShape::Number,
                ExpressionShape::TypedNumber,
                ExpressionShape::Block,
                ExpressionShape::Table,
                ExpressionShape::List,
                ExpressionShape::String,
            ];
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

fn trim_curly_braces(input: &Spanned<String>) -> Spanned<String> {
    let mut chars = input.chars();

    match (chars.next(), chars.next_back()) {
        (Some('{'), Some('}')) => chars
            .collect::<String>()
            .spanned(Span::new(input.span.start + 1, input.span.end - 1)),
        _ => input.clone(),
    }
}

fn trim_square_braces(input: &Spanned<String>) -> Spanned<String> {
    let mut chars = input.chars();

    match (chars.next(), chars.next_back()) {
        (Some('['), Some(']')) => chars
            .collect::<String>()
            .spanned(Span::new(input.span.start + 1, input.span.end - 1)),
        _ => input.clone(),
    }
}

fn trim_quotes(input: &str) -> String {
    let mut chars = input.chars();

    match (chars.next(), chars.next_back()) {
        (Some('\''), Some('\'')) => chars.collect(),
        (Some('"'), Some('"')) => chars.collect(),
        (Some('`'), Some('`')) => chars.collect(),
        _ => input.to_string(),
    }
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
    signature: &[ExpressionShape],
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

    let (head, error) = parse_expr(&call.elements[0], &ExpressionShape::Block, scope);
    let mut span = head.span;
    if err.is_none() {
        err = error;
    }

    let mut args = vec![];
    for arg in call.elements.iter().skip(1) {
        let (arg, error) = parse_expr(arg, &ExpressionShape::Any, scope);
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

fn parse_set_variable(
    call: LiteCommand,
    scope: &Scope,
) -> (Spanned<Expression>, Option<ParseError>) {
    if call.elements[0].item != "set" || call.elements.len() != 4 {
        return (
            garbage(call.elements[0].span),
            Some(ParseError::UnexpectedType {
                expected: "assignment".into(),
                span: call.elements[0].span,
            }),
        );
    }
    if call.elements[2].item != "=" {
        return (
            garbage(call.elements[2].span),
            Some(ParseError::UnexpectedType {
                expected: "equals".into(),
                span: call.elements[2].span,
            }),
        );
    }

    let variable = &call.elements[1];
    let (expr, err) = parse_expr(&call.elements[3], &ExpressionShape::Any, scope);

    (
        Expression::SetVariable(variable.item.clone(), Box::new(expr)).spanned(Span::new(
            call.elements[0].span.start,
            call.elements[3].span.end,
        )),
        err,
    )
}

fn parse_set_env_variable(
    call: LiteCommand,
    scope: &Scope,
) -> (Spanned<Expression>, Option<ParseError>) {
    if call.elements[0].item != "setenv" || call.elements.len() != 4 {
        return (
            garbage(call.elements[0].span),
            Some(ParseError::UnexpectedType {
                expected: "assignment".into(),
                span: call.elements[0].span,
            }),
        );
    }
    if call.elements[2].item != "=" {
        return (
            garbage(call.elements[2].span),
            Some(ParseError::UnexpectedType {
                expected: "equals".into(),
                span: call.elements[2].span,
            }),
        );
    }

    let variable = &call.elements[1];
    let (expr, err) = parse_expr(&call.elements[3], &ExpressionShape::Any, scope);

    (
        Expression::SetEnvVariable(variable.item.clone(), Box::new(expr)).spanned(Span::new(
            call.elements[0].span.start,
            call.elements[3].span.end,
        )),
        err,
    )
}

fn parse_definition(call: LiteCommand, scope: &mut Scope) -> Option<ParseError> {
    // A this point, we've already handled the prototype and put it into scope
    // So our main goal here is to parse the block now that the names and
    // prototypes of adjacent commands are also available

    if call.elements.len() == 4 {
        let name = &call.elements[1].item;
        let (block, err) = parse_block(&call.elements[3], false, scope);

        let block = match block.item {
            Expression::Block(_params, block) => {
                // TODO: throw an error if params are available
                block
            }
            _ => {
                return Some(ParseError::UnexpectedType {
                    expected: "block".into(),
                    span: block.span,
                });
            }
        };

        if let Some(slot) = scope.commands.get_mut(name) {
            slot.block = Some(block);
        }

        err
    } else {
        Some(ParseError::UnexpectedType {
            expected: "definition".into(),
            span: call.elements[0].span,
        })
    }
}

fn expand_aliases_in_call(call: &mut LiteCommand, scope: &Scope) {
    if let Some(name) = call.elements.get(0) {
        if let Some(mut expansion) = scope.get_alias(name) {
            call.elements.remove(0);
            expansion.append(&mut call.elements);
            call.elements = expansion;
        }
    }
}

fn parse_call(
    mut call: LiteCommand,
    scope: &mut Scope,
) -> (Spanned<Expression>, Option<ParseError>) {
    expand_aliases_in_call(&mut call, scope);
    if call.elements.is_empty() {
        (
            Expression::Garbage.spanned_unknown(),
            Some(ParseError::UnexpectedType {
                expected: "call".into(),
                span: Span::unknown(),
            }),
        )
    } else if call.elements[0].starts_with('^') {
        parse_external_call(call, scope)
    } else if call.elements[0].starts_with('$') || call.elements[0].starts_with('{') {
        parse_value_call(call, scope)
    } else if call.elements[0].item == "set" {
        parse_set_variable(call, scope)
    } else if call.elements[0].item == "setenv" {
        parse_set_env_variable(call, scope)
    } else if let Some(signature) = scope.get_signature(&call.elements[0].item) {
        parse_internal_call(call, &signature, scope)
    } else {
        parse_external_call(call, scope)
    }
}

fn parse_parameters(s: &Spanned<String>, scope: &Scope) -> (Vec<Parameter>, Option<ParseError>) {
    let mut err = None;

    let (preparsed_params, error) = parse_list(s, scope);
    if err.is_none() {
        err = error;
    }

    let mut params = vec![];
    for preparsed_param in preparsed_params {
        match preparsed_param.item {
            Expression::String(s) => {
                let parts: Vec<_> = s.split(':').collect();
                if parts.len() == 1 {
                    params.push(Parameter::new(parts[0].to_string(), ExpressionShape::Any));
                } else if parts.len() == 2 {
                    let name = parts[0].to_string();
                    let shape = match parts[1] {
                        "int" => ExpressionShape::Integer,
                        "string" => ExpressionShape::String,
                        "block" => ExpressionShape::Block,
                        "any" => ExpressionShape::Any,
                        _ => {
                            if err.is_none() {
                                err = Some(ParseError::UnexpectedType {
                                    expected: "param with known type".to_string(),
                                    span: preparsed_param.span,
                                });
                            }
                            ExpressionShape::Any
                        }
                    };
                    params.push(Parameter::new(name, shape));
                } else if err.is_none() {
                    err = Some(ParseError::UnexpectedType {
                        expected: "parameter with type".to_string(),
                        span: preparsed_param.span,
                    });
                }
            }
            _ => {
                if err.is_none() {
                    err = Some(ParseError::UnexpectedType {
                        expected: "parameter".to_string(),
                        span: preparsed_param.span,
                    });
                }
            }
        }
    }

    (params, err)
}

fn parse_definition_prototype(call: &LiteCommand, scope: &mut Scope) -> Option<ParseError> {
    let mut err = None;

    if call.elements.len() != 4 {
        return Some(ParseError::UnexpectedType {
            expected: "definition".into(),
            span: call.elements[0].span,
        });
    }

    if call.elements[0].item != "def" {
        return Some(ParseError::UnexpectedType {
            expected: "definition".into(),
            span: call.elements[0].span,
        });
    }

    let name = call.elements[1].item.clone();
    let (params, error) = parse_parameters(&call.elements[2], scope);
    if err.is_none() {
        err = error;
    }

    scope
        .commands
        .insert(name, CommandDefinition::new(params, None));

    err
}

fn parse_alias(call: LiteCommand, scope: &mut Scope) -> Option<ParseError> {
    if call.elements.len() < 4 {
        return Some(ParseError::UnexpectedType {
            expected: "definition".into(),
            span: call.elements[0].span,
        });
    }

    if call.elements[0].item != "alias" {
        return Some(ParseError::UnexpectedType {
            expected: "definition".into(),
            span: call.elements[0].span,
        });
    }

    if call.elements[2].item != "=" {
        return Some(ParseError::UnexpectedType {
            expected: "definition".into(),
            span: call.elements[0].span,
        });
    }

    let name = call.elements[1].item.clone();
    let args: Vec<_> = call.elements.into_iter().skip(3).collect();

    scope.aliases.insert(name, args);

    None
}

fn parse_helper(lite_block: LiteBlock, scope: &mut Scope) -> (ExpressionBlock, Option<ParseError>) {
    let mut output = ExpressionBlock::new();
    let mut err = None;

    // Check for custom commands first
    for group in lite_block.groups.iter() {
        for pipeline in &group.pipelines {
            for call in &pipeline.commands {
                if let Some(first) = call.elements.first() {
                    if first.item == "def" {
                        if pipeline.commands.len() > 1 && err.is_none() {
                            err = Some(ParseError::DefinitionInPipeline(first.span));
                        }
                        parse_definition_prototype(call, scope);
                    }
                }
            }
        }
    }

    // Then the rest of the code
    for group in lite_block.groups {
        let mut out_group = ExpressionGroup::new();
        for pipeline in group.pipelines {
            let mut out_pipe = ExpressionPipeline::new();
            for call in pipeline.commands {
                if call.elements[0].item == "def" {
                    let error = parse_definition(call, scope);
                    if err.is_none() {
                        err = error;
                    }
                } else if call.elements[0].item == "alias" {
                    let error = parse_alias(call, scope);
                    if err.is_none() {
                        err = error;
                    }
                } else {
                    let (parsed, error) = parse_call(call, scope);
                    if err.is_none() {
                        err = error;
                    }
                    out_pipe.push(parsed);
                }
            }
            if !out_pipe.pipeline.is_empty() {
                out_group.push(out_pipe);
            }
        }
        if !out_group.pipelines.is_empty() {
            output.push(out_group);
        }
    }

    output.definitions = scope.commands.clone();

    (output, err)
}

pub fn parse(
    input: &str,
    span_offset: usize,
    scope: &mut Scope,
) -> (ExpressionBlock, Option<ParseError>) {
    let (output, error) = lex(input, span_offset);
    if error.is_some() {
        return (ExpressionBlock::new(), error);
    }
    let (groups, error) = group(output);
    if error.is_some() {
        return (ExpressionBlock::new(), error);
    }

    parse_helper(groups, scope)
}
