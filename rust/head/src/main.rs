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
    let file = File::open(&path);
    if let Err(msg) = file {
        println!("Error: {}", msg);
        return;
    }

    let br = BufReader::new(file.ok().unwrap());
    for line in br.lines().take(10) {
        if let Ok(line) = line {
            println!("{}", line);
        }
    }
}
