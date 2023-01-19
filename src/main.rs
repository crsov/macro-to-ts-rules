#[macro_use]
extern crate log;

mod mbe;
mod to_dsl;

use clap::{command, Parser};
use mbe::MacroRules;
use proc_macro2::Ident;
use std::{collections::HashMap, path::PathBuf};
use syn::{Item, ItemMacro, ItemMod};
use syn_inline_mod::InlinerBuilder;

use crate::to_dsl::{macro_to_ts_dsl, one_of, quoted};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    files: Vec<PathBuf>,
}

fn main() {
    env_logger::init();

    let args = Cli::parse();

    let inliner = InlinerBuilder::default();

    let parsed_files = args.files.into_iter().flat_map(|f| {
        debug!("Processing crate root {f:#?}");
        inliner
            .parse_and_inline_modules(&f)
            .map_err(|e| warn!("Cannot parse file {f:#?}: {e}"))
            .map(|ir| ir.into_output_and_errors())
            .map(|(syn_file, errors)| {
                errors
                    .iter()
                    .for_each(|e| warn!("Error while inlining {f:#?}: {e}"));
                syn_file
            })
            .ok()
    });
    let results = parsed_files
        .flat_map(|pf| pf.items)
        .flat_map(|i| match i {
            Item::Mod(ItemMod {
                content: Some((_, items)),
                ..
            }) => items,
            i => vec![i],
        })
        .flat_map(|i| match i {
            Item::Macro(m @ ItemMacro { ident: Some(_), .. }) => {
                Some((m.ident.unwrap(), m.mac.parse_body::<MacroRules>().unwrap()))
            }
            _ => None,
        })
        .map(macro_to_ts_dsl);

    let mut output = String::new();

    let mut with_idents = HashMap::<Ident, Vec<String>>::new();

    results.for_each(|(rule, ident)| {
        if let Some(with_this_ident) = with_idents.get_mut(&ident) {
            with_this_ident.push(rule)
        } else {
            with_idents.insert(ident, vec![rule]);
        }
    });

    output.push_str(&format!(
                "module.exports = {{known_macro_invocation: $ => {}}}",
                one_of(with_idents.into_iter().map(|(ident, rules)| { let input_rule = one_of(rules); format!(
                    "seq({}, \"!\", choice(seq(\"(\", {}, \")\"), seq(\"{{\", {}, \"}}\"), seq(\"[\", {}, \"]\")))",
                    quoted(ident),
                    &input_rule,
                    &input_rule,
                    &input_rule
                )}))
            ));

    println!("{output}");
}
