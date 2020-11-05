use std::collections::HashMap;

mod language;
mod lite_parse;
mod parse;

use language::{ExpressionShape, Scope};
use parse::parse;

fn main() {
    //let input = "this 34\nis { another; \npipeline } ;";
    let input = "this 34\nis 101 ;";
    let mut commands = HashMap::new();
    commands.insert("this".to_string(), vec![ExpressionShape::Integer]);

    let scope = Box::new(Scope {
        parent: None,
        commands,
    });

    println!("{:#?}", parse(input, 0, &scope));
}
