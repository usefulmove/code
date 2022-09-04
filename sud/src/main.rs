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

    while !sud.is_solved() {
        for i in 0..9 {
            for j in 0..9 {
                if sud.board[i][j] == 0 {
                    match sud.solve_loc(i, j) {
                        Some(value) => sud.board[i][j] = value,
                        None => (), // do nothing
                    }
                }
            }
        }
    }
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

    fn is_solved(&self) -> bool {
        for i in 0..9 {
            for j in 0..9 {
                if self.board[i][j] == 0 {
                    return false;
                }
            }
        }
        true
    }

    fn solve_loc(&self, i: usize, j: usize) -> Option<u8> {
        TODO
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