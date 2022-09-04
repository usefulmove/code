use std::fmt;
use std::env;

fn main() {
    let mut sud: Sudoku = Sudoku::new();

    let mut args: Vec<String> = env::args().collect();
    args.remove(0);

    //println!("debug..args..{:?}", args);

    let mut level: usize = 0;
    args.iter().enumerate().for_each(|(i, s)| {
        if s == "_" {
            sud.board[level][i % 9] = 0;
        } else {
            sud.board[level][i % 9] = s.parse::<u8>().unwrap();
        }
        if i % 9 == 8 { level += 1; }
    });

    println!("{}", sud);
}

struct Sudoku {
    board: [[u8; 9]; 9],
}

impl Sudoku {
    fn new() -> Self {
        Self {
            board: [[0; 9]; 9],
        }
    }
}

impl fmt::Display for Sudoku {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "  {:?}\n  {:?}\n  {:?}\n  {:?}\n  {:?}\n  {:?}\n  {:?}\n  {:?}\n  {:?}",
            self.board[0],
            self.board[1],
            self.board[2],
            self.board[3],
            self.board[4],
            self.board[5],
            self.board[6],
            self.board[7],
            self.board[8],
        )
    }
}