use clj_format::print;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() == 1 {
        print("Usage: clj-format-cli <file1> <file2> ...");
        std::process::exit(1);
    }
    for filename in &args[1..] {
        let contents = std::fs::read_to_string(filename).unwrap();
        let formatted = clj_format::format(&contents);
        std::fs::write(filename, formatted).unwrap();
    }
}
