use std::env;
use std::error::Error;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Write;
use std::io::stderr;

fn main() {
    let mut args = env::args();
    if args.len() != 2 {
        println!("Usage: cargo run <FILE>");
        return;
    }

    let path = args.nth(1).unwrap();
    let input = File::open(&path);
    if let Err(msg) = input {
        let mut stderr = stderr();
        let msg = msg.description().as_bytes();
        stderr.write(msg).ok().expect("fail writing error");
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
