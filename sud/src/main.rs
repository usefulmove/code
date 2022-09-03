fn main() {
    let sudoku: Sudoku = Sudoku::new();

    println!("{:#?}", sudoku.puzzle);
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