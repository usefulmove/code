use std::fmt;
use std::env;

fn main() {
    let mut sud: Sudoku = Sudoku::new();

    let mut args: Vec<String> = env::args().collect();
    args.remove(0);

    println!("debug..args..{:?}", args);

    let mut level: usize = 0;
    args.iter().enumerate().for_each(|(i, s)| {
        if s == "_" {
            sud.puzzle[level][i % 9] = 99;
        } else {
            sud.puzzle[level][i % 9] = s.parse::<u8>().unwrap();
        }
        if i % 9 == 8 { level += 1; }
    });

    println!("{}", sud);
}

struct Sudoku {
    puzzle: [[u8; 9]; 9],
}

impl Sudoku {
    fn new() -> Self {
        Self {
            puzzle: [[0; 9]; 9],
        }
    }
}

impl fmt::Display for Sudoku {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "  {:?}\n  {:?}\n  {:?}\n  {:?}\n  {:?}\n  {:?}\n  {:?}\n  {:?}\n  {:?}",
            self.puzzle[0],
            self.puzzle[1],
            self.puzzle[2],
            self.puzzle[3],
            self.puzzle[4],
            self.puzzle[5],
            self.puzzle[6],
            self.puzzle[7],
            self.puzzle[8],
        )
    }
}