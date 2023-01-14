use std::fmt::Display;

use proc_macro2::{Delimiter, Ident, Literal, Punct, TokenStream, TokenTree};

use syn::ext::IdentExt;
use syn::parse::{Error, Parse, ParseBuffer, ParseStream, Result};
use syn::token::{Brace, Bracket, Dollar, Paren};
use syn::{braced, bracketed, parenthesized, Lifetime, Token};

#[derive(Debug)]
pub struct MacroRules {
    pub rules: Vec<Rule>,
}

#[derive(Debug)]
pub struct Rule {
    pub matcher: Vec<Matcher>,
    pub expansion: TokenStream,
}

#[derive(Debug)]
pub enum Matcher {
    Punct(Punct),
    Ident(Ident),
    Lifetime(Lifetime),
    Literal(Literal),
    Group {
        delimiter: Delimiter,
        content: Vec<Matcher>,
    },
    Repeat {
        content: Vec<Matcher>,
        separator: Option<Separator>,
        repetition: Repetition,
    },
    Fragment {
        name: Ident,
        classificator: Classificator,
    },
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, PartialOrd, Ord)]
pub enum Repetition {
    /// `$(...)*`
    Repeated,
    /// `$(...)+`
    AtLeastOnce,
    /// `$(...)?`
    AtMostOnce,
}

#[derive(Debug)]
pub enum Separator {
    Punct(Punct),
    Ident(Ident),
    Literal(Literal),
}

impl Display for Separator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Punct(x) => x.fmt(f),
            Self::Ident(x) => x.fmt(f),
            Self::Literal(x) => x.fmt(f),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, PartialOrd, Ord)]
pub enum Classificator {
    Ident,
    PatParam,
    Path,
    Expr,
    Ty,
    Pat,
    Stmt,
    Block,
    Item,
    Meta,
    Tt,
    Vis,
    Literal,
    Lifetime,
}

fn delimited(input: ParseStream<'_>) -> Result<(Delimiter, ParseBuffer<'_>)> {
    let content;
    let delimiter = if input.peek(Paren) {
        parenthesized!(content in input);
        Delimiter::Parenthesis
    } else if input.peek(Brace) {
        braced!(content in input);
        Delimiter::Brace
    } else if input.peek(Bracket) {
        bracketed!(content in input);
        Delimiter::Bracket
    } else {
        return Err(input.error("expected delimiter"));
    };
    Ok((delimiter, content))
}

impl Parse for MacroRules {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let rules = Rule::parse_many(input)?;

        Ok(MacroRules { rules })
    }
}

impl Rule {
    fn parse_many(input: ParseStream<'_>) -> Result<Vec<Self>> {
        let rules = input.parse_terminated::<Rule, Token![;]>(Rule::parse)?;
        if rules.is_empty() {
            Err(input.error("expected at least one macro rule"))
        } else {
            Ok(rules.into_iter().collect())
        }
    }
}

impl Parse for Rule {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        // Parse the input pattern.
        let content = delimited(input)?.1;
        let matcher = Matcher::parse_many(&content)?;

        input.parse::<Token![=>]>()?;

        // Parse the expansion tokens.
        let content = delimited(input)?.1;
        let expansion: TokenStream = content.parse()?;

        Ok(Rule { matcher, expansion })
    }
}

impl Matcher {
    fn parse_many(input: ParseStream<'_>) -> Result<Vec<Self>> {
        let mut matchers = Vec::new();
        while !input.is_empty() {
            matchers.push(input.parse()?);
        }
        Ok(matchers)
    }
}

impl Parse for Matcher {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        if input.peek(Paren) || input.peek(Bracket) || input.peek(Brace) {
            let (delimiter, content) = delimited(input)?;
            let content = Matcher::parse_many(&content)?;
            Ok(Matcher::Group { delimiter, content })
        } else if input.parse::<Option<Dollar>>()?.is_some() {
            if input.peek(Paren) {
                let content;
                parenthesized!(content in input);
                let content = Matcher::parse_many(&content)?;
                let separator = Separator::parse_optional(input)?;
                let repetition: Repetition = input.parse()?;
                if repetition == Repetition::AtMostOnce && separator.is_some() {
                    return Err(
                        input.error("the `?` macro repetition operator does not take a separator")
                    );
                }
                Ok(Matcher::Repeat {
                    content,
                    separator,
                    repetition,
                })
            } else {
                let name = Ident::parse_any(input)?;
                input.parse::<Token![:]>()?;
                let fragment: Classificator = input.parse()?;
                Ok(Matcher::Fragment {
                    name,
                    classificator: fragment,
                })
            }
        } else if let Some(lifetime) = input.parse()? {
            Ok(Matcher::Lifetime(lifetime))
        } else {
            match input.parse()? {
                TokenTree::Ident(ident) => Ok(Matcher::Ident(ident)),
                TokenTree::Punct(punct) => Ok(Matcher::Punct(punct)),
                TokenTree::Literal(literal) => Ok(Matcher::Literal(literal)),
                TokenTree::Group(_) => unreachable!(),
            }
        }
    }
}

impl Separator {
    fn parse_optional(input: ParseStream<'_>) -> Result<Option<Self>> {
        if input.peek(Token![*]) || input.peek(Token![+]) || input.peek(Token![?]) {
            Ok(None)
        } else {
            input.parse().map(Some)
        }
    }
}

impl Parse for Separator {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(match input.parse()? {
            TokenTree::Ident(ident) => Separator::Ident(ident),
            // FIXME: multi-character punctuation
            TokenTree::Punct(punct) => Separator::Punct(punct),
            TokenTree::Literal(literal) => Separator::Literal(literal),
            TokenTree::Group(group) => {
                return Err(Error::new(group.span(), "unexpected token"));
            }
        })
    }
}

impl Parse for Repetition {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        if input.parse::<Option<Token![*]>>()?.is_some() {
            Ok(Repetition::Repeated)
        } else if input.parse::<Option<Token![+]>>()?.is_some() {
            Ok(Repetition::AtLeastOnce)
        } else if input.parse::<Option<Token![?]>>()?.is_some() {
            Ok(Repetition::AtMostOnce)
        } else {
            Err(input.error("expected `*` or `+` or `?`"))
        }
    }
}

impl Parse for Classificator {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let ident: Ident = input.parse()?;
        match ident.to_string().as_str() {
            "ident" => Ok(Classificator::Ident),
            "path" => Ok(Classificator::Path),
            "expr" => Ok(Classificator::Expr),
            "ty" => Ok(Classificator::Ty),
            "pat_param" => Ok(Classificator::PatParam),
            "pat" => Ok(Classificator::Pat),
            "stmt" => Ok(Classificator::Stmt),
            "block" => Ok(Classificator::Block),
            "item" => Ok(Classificator::Item),
            "meta" => Ok(Classificator::Meta),
            "tt" => Ok(Classificator::Tt),
            "vis" => Ok(Classificator::Vis),
            "literal" => Ok(Classificator::Literal),
            "lifetime" => Ok(Classificator::Lifetime),
            _ => Err(Error::new(ident.span(), "unrecognized fragment specifier")),
        }
    }
}