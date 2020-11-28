use std::iter::Peekable;
use std::str::CharIndices;

use crate::language::{BlockKind, ParseError, Span, Spanned, SpannedItem, Token, TokenContents};

pub type Input<'t> = Peekable<CharIndices<'t>>;

/// Finds the extents of a bare (un-classified) token, returning the string with its associated span,
/// along with any parse error that was discovered along the way.
/// Bare tokens are unparsed content separated by spaces or a command separator (like pipe or semicolon)
/// Bare tokens may be surrounded by quotes (single, double, or backtick) or braces (square, paren, curly)
pub fn bare(src: &mut Input, span_offset: usize) -> (Spanned<String>, Option<ParseError>) {
    let mut bare = String::new();
    let start_offset = if let Some((pos, _)) = src.peek() {
        *pos
    } else {
        0
    };

    let mut inside_quote: Option<char> = None;
    let mut block_level: Vec<BlockKind> = vec![];

    while let Some((_, c)) = src.peek() {
        let c = *c;
        if inside_quote.is_some() {
            if Some(c) == inside_quote {
                inside_quote = None;
            }
        } else if c == '\'' || c == '"' || c == '`' {
            inside_quote = Some(c);
        } else if c == '[' {
            block_level.push(BlockKind::SquareBracket);
        } else if c == ']' {
            if let Some(BlockKind::SquareBracket) = block_level.last() {
                let _ = block_level.pop();
            }
        } else if c == '{' {
            block_level.push(BlockKind::CurlyBracket);
        } else if c == '}' {
            if let Some(BlockKind::CurlyBracket) = block_level.last() {
                let _ = block_level.pop();
            }
        } else if c == '(' {
            block_level.push(BlockKind::Paren);
        } else if c == ')' {
            if let Some(BlockKind::Paren) = block_level.last() {
                let _ = block_level.pop();
            }
        } else if block_level.is_empty() && (c.is_whitespace() || c == '|' || c == ';' || c == '#')
        {
            break;
        }
        bare.push(c);
        let _ = src.next();
    }

    let span = Span::new(
        start_offset + span_offset,
        start_offset + span_offset + bare.len(),
    );

    if let Some(block) = block_level.last() {
        let delim: char = (*block).into();
        let cause = ParseError::UnexpectedEof(delim.to_string(), span);

        while let Some(bk) = block_level.pop() {
            bare.push(bk.into());
        }

        return (bare.spanned(span), Some(cause));
    }

    if let Some(delimiter) = inside_quote {
        // The non-lite parse trims quotes on both sides, so we add the expected quote so that
        // anyone wanting to consume this partial parse (e.g., completions) will be able to get
        // correct information from the non-lite parse.
        bare.push(delimiter);

        return (
            bare.spanned(span),
            Some(ParseError::UnexpectedEof(delimiter.to_string(), span)),
        );
    }

    if bare.is_empty() {
        return (
            bare.spanned(span),
            Some(ParseError::UnexpectedEof("command".to_string(), span)),
        );
    }

    (bare.spanned(span), None)
}

fn skip_comment(input: &mut Input) {
    for (_, c) in input {
        if c == '\n' || c == '\r' {
            break;
        }
    }
}

/// Breaks the input string into a vector of tokens. This tokenization only tries to classify separators like
/// semicolons, pipes, etc from external bare values (values that haven't been classified further)
/// Takes in a string and and offset, which is used to offset the spans created (for when this function is used to parse inner strings)
pub fn lex(input: &str, span_offset: usize) -> (Vec<Token>, Option<ParseError>) {
    let mut char_indices = input.char_indices().peekable();
    let mut error = None;

    let mut output = vec![];
    let mut is_complete = true;

    while let Some((idx, c)) = char_indices.peek() {
        if *c == '|' {
            let idx = *idx;
            let prev_idx = idx;
            let _ = char_indices.next();
            if let Some((idx, c)) = char_indices.peek() {
                if *c == '|' {
                    // we have '||' instead of '|'
                    let idx = *idx;
                    let _ = char_indices.next();
                    output.push(Token::new(
                        TokenContents::Bare("||".into()),
                        Span::new(span_offset + prev_idx, span_offset + idx + 1),
                    ));
                    continue;
                }
            }
            output.push(Token::new(
                TokenContents::Pipe,
                Span::new(span_offset + idx, span_offset + idx + 1),
            ));
            is_complete = false;
        } else if *c == ';' {
            if !is_complete && error.is_none() {
                error = Some(ParseError::UnexpectedSemicolon(Span::new(*idx, idx + 1)));
            }
            let idx = *idx;
            let _ = char_indices.next();
            output.push(Token::new(
                TokenContents::Semicolon,
                Span::new(span_offset + idx, span_offset + idx + 1),
            ));
        } else if *c == '\n' || *c == '\r' {
            let idx = *idx;
            let _ = char_indices.next();
            output.push(Token::new(
                TokenContents::EOL,
                Span::new(span_offset + idx, span_offset + idx + 1),
            ));
        } else if *c == '#' {
            skip_comment(&mut char_indices);
        } else if c.is_whitespace() {
            let _ = char_indices.next();
        } else {
            let (result, err) = bare(&mut char_indices, span_offset);
            if error.is_none() {
                error = err;
            }
            is_complete = true;
            let Spanned { item, span } = result;
            output.push(Token::new(TokenContents::Bare(item), span));
        }
    }

    (output, error)
}
