use std::fmt::Display;

use proc_macro2::Ident;

use crate::mbe::{Classificator, MacroRules, Matcher, Repetition, Rule, Separator};

macro_rules! use_rule {
    ($name:ident, $dsl_rule:ident) => {
        const $name: &str = concat!("$.", stringify!($dsl_rule));
    };
}

use_rule!(LIFETIME_RULE, lifetime);
use_rule!(TYPE_RULE, _type);
use_rule!(TOKENTREE_RULE, delim_token_tree);
// TODO: Use non-crate visibility modifier
use_rule!(VISIBILITY_RULE, visibility_modifier);
use_rule!(PATH_RULE, _path);
use_rule!(PATTERN_RULE, _pattern);
// TODO: Is this ok?
use_rule!(PATTERN_PARAMETER_RULE, _pattern);
use_rule!(STATEMENT_RULE, _statement);
use_rule!(LITERAL_RULE, _literal);
use_rule!(BLOCK_RULE, block);
// TODO: Is this ok?
use_rule!(IDENT_RULE, identifier);
use_rule!(EXPR_RULE, _expression);

const ITEM_RULE: &str = "choice(choice($.macro_invocation, $.macro_definition), seq(optional($.visibility_modifier), $._declaration_statement), $.attribute_item)";
// TODO: Is this ok?
const META_RULE: &str = "choice($.inner_attribute_item, $.attribute_item, choice(
            seq(\"=\", field(\"value\", $._expression)),
            field(\"arguments\", alias($.delim_token_tree, $.token_tree))
          ))";

pub fn macro_to_ts_dsl(macro_: (Ident, MacroRules)) -> (String, String, Ident) {
    let dsl_rule_name = format!("macro_{}_{}_input", macro_.0, uuid::Uuid::new_v4().simple());
    let mut res = format!("{dsl_rule_name}: $ => ");
    res.push_str(&one_of(macro_.1.rules.into_iter().map(
        |Rule {
             matcher,
             expansion: _,
         }| matchers_to_dsl_rule(matcher),
    )));
    (res, dsl_rule_name, macro_.0)
}

pub fn one_of<T: IntoIterator<Item = String>>(dsl_rules: T) -> String {
    let mut res = "choice(".to_string();
    let mut rs = dsl_rules.into_iter();
    if let Some(r) = rs.next() {
        res.push_str(&r);
        res.push_str(&rs.map(|r| format!(", {r}")).collect::<String>());
    }
    res.push(')');
    res
}

pub fn quoted<T: Display>(string: T) -> String {
    format!("\"{string}\"")
}

pub fn matchers_to_dsl_rule<T: IntoIterator<Item = Matcher>>(matchers: T) -> String {
    let mut res = "seq(".to_string();
    let mut ms = matchers.into_iter();
    if let Some(m) = ms.next() {
        res.push_str(&matcher_to_dsl_rule(m));
        res.push_str(
            &ms.map(|m| format!(", {}", matcher_to_dsl_rule(m)))
                .collect::<String>(),
        );
    }
    res.push(')');
    res
}

pub fn matcher_to_dsl_rule(matcher: Matcher) -> String {
    match matcher {
        Matcher::Punct(p) => quoted(p.as_char()),
        Matcher::Ident(i) => quoted(i),
        Matcher::Literal(l) => quoted(l),
        Matcher::Group {
            delimiter: _,
            content,
        } => matchers_to_dsl_rule(content),
        Matcher::Lifetime(l) => quoted(l),
        Matcher::Repeat {
            content,
            separator,
            repetition,
        } => repetition_matcher_to_dsl_rule(content, separator, repetition),
        Matcher::Fragment {
            name: _,
            classificator,
        } => match classificator {
            Classificator::Lifetime => LIFETIME_RULE,
            Classificator::Ty => TYPE_RULE,
            Classificator::Tt => TOKENTREE_RULE,
            Classificator::Pat => PATTERN_RULE,
            Classificator::Vis => VISIBILITY_RULE,
            Classificator::Path => PATH_RULE,
            Classificator::Expr => EXPR_RULE,
            Classificator::Literal => LITERAL_RULE,
            Classificator::Item => ITEM_RULE,
            Classificator::Block => BLOCK_RULE,
            Classificator::PatParam => PATTERN_PARAMETER_RULE,
            Classificator::Ident => IDENT_RULE,
            Classificator::Meta => META_RULE,
            Classificator::Stmt => STATEMENT_RULE,
        }
        .to_string(),
    }
}

fn repetition_matcher_to_dsl_rule(
    content: Vec<Matcher>,
    separator: Option<Separator>,
    repetition: Repetition,
) -> String {
    let mut res = match repetition {
        Repetition::AtMostOnce => "optional(",
        Repetition::AtLeastOnce => "repeat1(",
        Repetition::Repeated => "repeat(",
    }
    .to_string();
    res.push_str(&if let Some(s) = separator {
        format!(
            "seq({}, {})",
            matchers_to_dsl_rule(content),
            quoted(s.to_string())
        )
    } else {
        matchers_to_dsl_rule(content)
    });
    res.push(')');
    res
}
