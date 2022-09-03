use std::fmt;

fn main() {
    let mut sud: Sudoku = Sudoku::new();

    sud.puzzle[4][4] = 9;

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