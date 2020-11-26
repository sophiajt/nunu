use std::{collections::HashMap, io::Write};

mod language;
mod lite_parse;
mod parse;

use language::{ExpressionShape, Scope};
use parse::parse;
use std::io;

fn main() {
    let mut input = String::new();
    loop {
        print!("> ");
        let _ = std::io::stdout().flush();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                if input.trim() == "quit" || input.trim() == "exit" {
                    break;
                }
                let mut commands = HashMap::new();
                commands.insert(
                    "this".to_string(),
                    vec![ExpressionShape::Integer, ExpressionShape::Any],
                );

                let mut scope = Box::new(Scope {
                    parent: None,
                    commands,
                });

                println!("{:#?}", parse(&input, 0, &mut scope));
                input.clear();
            }
            Err(error) => {
                println!("error: {}", error);
                break;
            }
        }
    }
    //let input = "this 3 a\nis { another; \npipeline } ; { [y] foo bar } 3";
    //let input = "?+󁃬z i￳.　";
}
