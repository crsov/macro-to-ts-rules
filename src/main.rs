mod mbe;
mod to_dsl;

use clap::{command, Parser, Subcommand};
use mbe::MacroRules;
use proc_macro2::Ident;
use std::{collections::HashMap, path::PathBuf};
use syn::{Item, ItemMacro, ItemMod};
use syn_inline_mod::parse_and_inline_modules;

use crate::to_dsl::{macro_to_ts_dsl, one_of, quoted};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    #[command(arg_required_else_help = true)]
    Process { file: PathBuf },
}

fn main() {
    let args = Cli::parse();

    match args.command {
        Command::Process { file } => {
            let parsed_file = parse_and_inline_modules(&file);
            let results = parsed_file
                .items
                .into_iter()
                .flat_map(|i| match i {
                    Item::Mod(ItemMod {
                        content: Some((_, items)),
                        ..
                    }) => items,
                    i => vec![i],
                })
                .filter_map(|i| match i {
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
    }
}
