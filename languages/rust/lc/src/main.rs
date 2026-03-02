fn main() {
    let filename = std::env::args().nth(1).expect("no file given");
    let read_result = std::fs::read_to_string(&filename);

    match read_result {
        Ok(contents) => {
            println!("  file:  {}", filename);
            println!("  lines: {}", contents.lines().count());
            println!("  words: {}", contents.split_whitespace().count());
        },
        Err(e) => println!("  error: {}", e),
    }
}
