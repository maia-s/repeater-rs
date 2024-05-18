//! # repeater
//!
//! This crate provides the [`repeat!`] macro.

use proc_macro::{token_stream, Delimiter, Group, Literal, Spacing, Span, TokenStream, TokenTree};
use std::borrow::Cow;

#[derive(Clone, Copy)]
struct Sigil {
    char: char,
    len: usize,
}

///
/// ```rust
/// # use repeater::repeat;
/// let n = repeat!(5 => 0 #( + 1 )*);
/// assert_eq!(n, 5);
/// ```
///
/// ```rust
/// # use repeater::repeat;
/// let n = repeat!(###: 5 => 0 ###( + 1 )*);
/// assert_eq!(n, 5);
/// ```
///
/// ```rust
/// # use repeater::repeat;
/// let n = repeat!(#i: 5 => 0 #(+ #i)*);
/// assert_eq!(n, 10);
/// ```
///
/// ```rust
/// # use repeater::repeat;
/// let n = repeat!(#i: 5 => #(#i)+*);
/// assert_eq!(n, 10);
/// ```
///
/// ```rust
/// # use repeater::repeat;
/// let i = -1;
/// let n = repeat!(#i: 5 => [#(#i, i),*]);
/// assert_eq!(n, [0, -1, 1, -1, 2, -1, 3, -1, 4, -1]);
/// ```
///
/// ```rust
/// # use repeater::repeat;
/// let tuple = repeat!(#i: 2 => (#(repeat!(##j: 2 => (##((#i, ##j)),*))),*));
/// assert_eq!(tuple, (((0, 0), (0, 1)), ((1, 0), (1, 1))));
/// ```
///
/// ```rust
/// # use repeater::repeat;
/// let tuple = repeat!(#i: 2 => repeat!(##j: 2 => (#(##((#i, ##j),)*)*)));
/// assert_eq!(tuple, ((0, 0), (0, 1), (1, 0), (1, 1)));
/// ```
#[proc_macro]
pub fn repeat(input: TokenStream) -> TokenStream {
    let mut input = ts_iter_fix(input);
    let mut next = input.next();

    let mut need_colon = false;

    let sigil = if let Some(TokenTree::Punct(p)) = next {
        need_colon = true;
        let char = p.as_char();
        let mut len = 1;
        next = input.next();
        let mut spacing = p.spacing();
        while spacing == Spacing::Joint {
            if let Some(TokenTree::Punct(ref p2)) = next {
                if p2.as_char() == char {
                    len += 1;
                    spacing = p2.spacing();
                    next = input.next();
                } else {
                    break;
                }
            } else {
                return Error::new(p.span(), "joint spaced punct wasn't followed by punct").into();
            }
        }
        Sigil { char, len }
    } else {
        Sigil { char: '#', len: 1 }
    };

    let loop_var = if let Some(TokenTree::Ident(ident)) = next {
        need_colon = true;
        next = input.next();
        Some(ident.to_string())
    } else {
        None
    };

    'colon: {
        if need_colon {
            if let Some(TokenTree::Punct(p)) = &next {
                if p.spacing() == Spacing::Alone && p.as_char() == ':' {
                    next = input.next();
                    break 'colon;
                }
            }
            return Error::new(
                next.map(|t| t.span()).unwrap_or_else(Span::call_site),
                "expected `:` after sigil/loop variable",
            )
            .into();
        }
    }

    let Some(TokenTree::Literal(repeat_count)) = &next else {
        return Error::new(
            next.map(|t| t.span()).unwrap_or_else(Span::call_site),
            "expected integer literal as repeat count",
        )
        .into();
    };
    let Ok(repeat_count) = repeat_count.to_string().parse::<usize>() else {
        return Error::new(
            next.unwrap().span(),
            "expected integer literal as repeat count",
        )
        .into();
    };

    next = input.next();
    let Some(TokenTree::Punct(p0)) = next else {
        return Error::new(
            next.map(|t| t.span()).unwrap_or_else(Span::call_site),
            "expected `=>` after repeat count",
        )
        .into();
    };
    if p0.spacing() != Spacing::Joint || p0.as_char() != '=' {
        return Error::new(p0.span(), "expected `=>` after repeat count").into();
    }
    next = input.next();
    let Some(TokenTree::Punct(p1)) = next else {
        return Error::new(
            next.map(|t| t.span()).unwrap_or_else(Span::call_site),
            "expected `=>` after repeat count",
        )
        .into();
    };
    if p1.spacing() != Spacing::Alone || p1.as_char() != '>' {
        return Error::new(p1.span(), "expected `=>` after repeat count").into();
    }

    let mut output = TokenStream::new();

    if let Err(e) = process(&mut output, &mut input, sigil, &|token, output, input| {
        if let TokenTree::Group(group) = &token {
            if group.delimiter() == Delimiter::Parenthesis {
                let delim = input.next();
                let Some(TokenTree::Punct(p)) = &delim else {
                    return Err(Error::new(
                        delim.map(|t| t.span()).unwrap_or_else(|| group.span()),
                        "expected delimiter or `*` after closing parenthesis for loop",
                    ));
                };
                let delim = if p.as_char() != '*' {
                    let Some(TokenTree::Punct(p)) = input.next() else {
                        return Err(Error::new(p.span(), "expected `*` after loop delimiter"));
                    };
                    if p.as_char() != '*' {
                        return Err(Error::new(p.span(), "expected `*` after loop delimiter"));
                    }
                    delim
                } else {
                    None
                };
                let group = ts_iter_fix(group.stream());
                for i in 0..repeat_count {
                    let mut group = group.clone();

                    if i == 0 {
                    } else if let Some(delim) = &delim {
                        output.extend([delim.clone()]);
                    }

                    process(output, &mut group, sigil, &|token, output, _input| {
                        if let TokenTree::Ident(ident) = token {
                            let ident_s = ident.to_string();
                            if Some(&ident_s) == loop_var.as_ref() {
                                output.extend([TokenTree::Literal(Literal::usize_unsuffixed(i))]);
                            } else {
                                return Err(Error::new(
                                    ident.span(),
                                    format!("{ident_s} isn't a loop index"),
                                ));
                            }
                        } else if let TokenTree::Group(_) = token {
                            return Err(Error::new(token.span(), "can't loop in loop"));
                        } else {
                            let s = String::from(sigil.char).repeat(sigil.len) + &token.to_string();
                            return Err(Error::new(
                                token.span(),
                                format!("invalid sigiled token: `{s}`"),
                            ));
                        }
                        Ok(())
                    })?;
                }
                return Ok(());
            }
        } else if let TokenTree::Ident(_) = token {
            return Err(Error::new(
                token.span(),
                "can't access loop index outside loop",
            ));
        }
        let s = String::from(sigil.char).repeat(sigil.len) + &token.to_string();
        Err(Error::new(
            token.span(),
            format!("invalid sigiled token: `{s}`"),
        ))
    }) {
        return e.into();
    }

    output
}

struct Error(Span, Cow<'static, str>);

impl Error {
    pub fn new(span: Span, message: impl Into<Cow<'static, str>>) -> Self {
        Self(span, message.into())
    }
}

impl From<Error> for TokenStream {
    fn from(value: Error) -> Self {
        let tokens: TokenStream = format!("compile_error!({:?})", value.1).parse().unwrap();
        let mut ts = TokenStream::new();
        ts.extend(tokens.into_iter().map(|mut tt| {
            tt.set_span(value.0);
            tt
        }));
        ts
    }
}

fn ts_iter_fix(ts: TokenStream) -> TsIter {
    TsIter(ts.into_iter())
}

#[derive(Clone)]
struct TsIter(token_stream::IntoIter);

impl Iterator for TsIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(flatten_token_tree)
    }
}

fn flatten_token_tree(tt: TokenTree) -> TokenTree {
    if let TokenTree::Group(group) = &tt {
        if group.delimiter() == Delimiter::None {
            let mut it = group.stream().into_iter();
            if let Some(token) = it.next() {
                if it.next().is_none() {
                    return flatten_token_tree(token);
                }
            }
        }
    }
    tt
}

fn process(
    output: &mut TokenStream,
    input: &mut TsIter,
    sigil: Sigil,
    handle: &impl Fn(TokenTree, &mut TokenStream, &mut TsIter) -> Result<(), Error>,
) -> Result<(), Error> {
    let mut accept_sigil = true;
    let mut sigil_buf = Vec::with_capacity(sigil.len);

    while let Some(token) = input.next() {
        if let TokenTree::Punct(p) = &token {
            if accept_sigil && p.as_char() == sigil.char {
                accept_sigil = p.spacing() == Spacing::Joint;
                sigil_buf.push(token);
                continue;
            }
        }

        accept_sigil = true;

        if !sigil_buf.is_empty() {
            if sigil_buf.len() == sigil.len {
                sigil_buf.clear();
                handle(token, output, input)?;
                continue;
            }
            output.extend(sigil_buf.drain(..));
        }

        if let TokenTree::Group(group) = &token {
            let mut group_output = TokenStream::new();
            let mut input = ts_iter_fix(group.stream());
            process(&mut group_output, &mut input, sigil, handle)?;
            output.extend([TokenTree::Group(Group::new(
                group.delimiter(),
                group_output,
            ))]);
        } else {
            output.extend([token]);
        }
    }

    Ok(())
}
