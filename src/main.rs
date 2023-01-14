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

            let mut with_idents = HashMap::<Ident, Vec<(String, String)>>::new();

            results.for_each(|(rule_definition, rule_name, ident)| {
                if let Some(with_this_ident) = with_idents.get_mut(&ident) {
                    with_this_ident.push((rule_definition, rule_name))
                } else {
                    with_idents.insert(ident, vec![(rule_definition, rule_name)]);
                }
            });

            let mut generic_rules = vec![];

            with_idents
                .into_iter()
                .for_each(|(ident, with_this_ident)| {
                    let mut local_rule_names = vec![];
                    with_this_ident
                        .into_iter()
                        .for_each(|(rule_definition, rule_name)| {
                            output.push_str(&format!("{rule_definition},"));
                            local_rule_names.push(rule_name);
                        });
                    let generic_rule_name = format!("macro_{}_input", &ident);
                    generic_rules.push((generic_rule_name.clone(), ident));
                    output.push_str(&format!(
                        "{generic_rule_name}: $ => {},",
                        one_of(local_rule_names.into_iter().map(|rn| format!("$.{rn}")))
                    ))
                });

            output.push_str(&format!(
                "known_macro_invocation: $ => prec(2, {}),",
                one_of(
                    generic_rules
                        .into_iter()
                        .map(|(rn, ident)| format!("seq({}, \"!\", $.{rn})", quoted(ident)))
                )
            ));

            println!("{output}");
        }
    }
}
