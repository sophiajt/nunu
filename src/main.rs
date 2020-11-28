use std::{collections::HashMap, io::Write};

mod language;
mod lex;
mod parse;

use language::{CommandDefinition, ExpressionShape, Parameter, Scope};
use parse::parse;
use std::io;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().collect();

    let mut commands = HashMap::new();
    commands.insert(
        "echo".to_string(),
        CommandDefinition::new(vec![Parameter::new("b".into(), ExpressionShape::Any)], None),
    );

    let scope = Box::new(Scope {
        parent: None,
        commands,
        aliases: HashMap::new(),
    });

    let mut scope = Box::new(Scope {
        parent: Some(&scope),
        commands: HashMap::new(),
        aliases: HashMap::new(),
    });

    if args.len() < 2 {
        let mut input = String::new();
        loop {
            print!("> ");
            let _ = std::io::stdout().flush();
            match io::stdin().read_line(&mut input) {
                Ok(_) => {
                    if input.trim() == "quit" || input.trim() == "exit" {
                        break;
                    }

                    println!("{:#?}", parse(&input, 0, &mut scope));
                    input.clear();
                }
                Err(error) => {
                    println!("error: {}", error);
                    break;
                }
            }
        }
    } else {
        for arg in args.into_iter().skip(1) {
            let contents = std::fs::read_to_string(arg)?;

            println!("{:#?}", parse(&contents, 0, &mut scope));
        }
    }
    //let input = "this 3 a\nis { another; \npipeline } ; { [y] foo bar } 3";
    //let input = "?+󁃬z i￳.　";
    Ok(())
}
