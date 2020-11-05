use std::iter::Peekable;
use std::str::CharIndices;

use crate::language::{BlockKind, ParseError, Span, Spanned, SpannedItem, Token, TokenContents};

pub type Input<'t> = Peekable<CharIndices<'t>>;

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
        } else if block_level.is_empty() && (c.is_whitespace() || c == '|' || c == ';') {
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

pub fn lex(input: &str, span_offset: usize) -> (Vec<Token>, Option<ParseError>) {
    let mut char_indices = input.char_indices().peekable();
    let mut error = None;

    let mut output = vec![];

    while let Some((idx, c)) = char_indices.peek() {
        if *c == '|' {
            let idx = *idx;
            let _ = char_indices.next();
            output.push(Token::new(
                TokenContents::Pipe,
                Span::new(span_offset + idx, span_offset + idx + 1),
            ));
        } else if *c == ';' {
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
        } else if *c == ' ' || *c == '\t' {
            let _ = char_indices.next();
        } else {
            let (result, err) = bare(&mut char_indices, span_offset);
            if error.is_none() {
                error = err;
            }
            let Spanned { item, span } = result;
            output.push(Token::new(TokenContents::Bare(item), span));
        }
    }

    (output, error)
}
