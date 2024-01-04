use std::env;
mod compiler;
mod lexer;

fn main() {
    let source = env::args().skip(1).next().unwrap();
    compiler::compile(source);
}
