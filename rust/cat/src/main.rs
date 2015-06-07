use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn main() {
    let mut args = env::args();
    if args.len() != 2 {
        println!("Usage: cargo run <FILE>");
        return;
    }

    let path = args.nth(1).unwrap();
    let input = File::open(&path);
    if let Err(msg) = input {
        println!("{}: {}", path, msg);
        return;
    }

    let file = input.ok().unwrap();
    let br = BufReader::new(file);
    for line in br.lines() {
        match line {
            Ok(line) => println!("{}", line),
            Err(msg) => {
                println!("{}", msg);
                return;
            }
        }
    }
}
