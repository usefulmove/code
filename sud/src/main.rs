use std::collections::HashSet;
use std::{env, fmt};

fn main() {
    let mut sud: Sudoku = Sudoku::new();

    let mut args: Vec<String> = env::args().collect();
    args.remove(0);

    //println!("debug..args..{:?}", args);

    let mut level: usize = 0;
    for (j, s) in args.iter().enumerate() {
        if s == "_" {
            sud.board[level][j % 9] = 0;
        } else {
            sud.board[level][j % 9] = s.parse::<u8>().unwrap();
        }
        if j % 9 == 8 { level += 1; }
    }

    println!(" input board:\n\n{}", sud);

    // solve
    sud.solve();

    println!("\n output board:\n\n{}", sud);
}

struct Sudoku {
    board: [[u8; 9]; 9],
}

#[derive(PartialEq, Eq)]
enum Group {
    A, B, C, D, E, F, G, H, I
}

impl Sudoku {
    fn new() -> Self {
        Self { board: [[0; 9]; 9] }
    }

    fn is_solved(&self) -> bool {
        self.board.iter()
            .flatten()
            .all(|node| *node != 0 )
    }

    fn is_possible(&self, n: u8, a: usize, b: usize) -> bool {
        let mut column_set: Set = Set::new();
        let mut row_set: Set = Set::new();
        let mut group_set: Set = Set::new();

        // build sets
        for i in 0..9 {
            for j in 0..9 {
                // ignore empty cells
                if self.board[i][j] == 0 { continue }

                // same column
                if j == b { column_set.insert(self.board[i][j]); }

                // same row
                if i == a { row_set.insert(self.board[i][j]); }

                // same group
                if self.group(i, j) == self.group(a, b) {
                    group_set.insert(self.board[i][j]);
                }
            }
        }

        // check for solution ( set comparison )
        let complete_set: Set = (1..=9).collect();
        let check_set: Set = column_set
            .union(&row_set)
            .cloned()
            .collect::<Set>()
            .union(&group_set)
            .cloned()
            .collect::<Set>();

        let possible: Set = complete_set
            .difference(&check_set)
            .cloned()
            .collect();

        possible.contains(&n)
    }

    /* recursive solver */
    fn solve(&mut self) {
        for i in 0..9 {
            for j in 0..9 {
                if self.board[i][j] == 0 {
                    for test_val in 1..=9 {
                        if self.is_possible(test_val, i, j) {
                            self.board[i][j] = test_val;
                            self.solve();
                            if self.is_solved() { return }
                            self.board[i][j] = 0; // reset current cell to 0
                        }
                    }
                    return;
                }
            }
        }
    }

    fn group(&self, a: usize, b: usize) -> Group {
        match (a, b) {
            (0..=2, 0..=2) => Group::A,
            (0..=2, 3..=5) => Group::B,
            (0..=2, 6..=8) => Group::C,
            (3..=5, 0..=2) => Group::D,
            (3..=5, 3..=5) => Group::E,
            (3..=5, 6..=8) => Group::F,
            (6..=8, 0..=2) => Group::G,
            (6..=8, 3..=5) => Group::H,
            (6..=8, 6..=8) => Group::I,
            _ => panic!("invalid group"),
        }
    }

}

impl fmt::Display for Sudoku {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut level: usize = 0;
        for n in 0..81 {
            write!(f, " {}", self.board[level][n % 9]).unwrap();
            if n % 9 == 8 {
                writeln!(f).unwrap();
                level += 1;
            }
        }
        Ok(())
    }
}

type Set = HashSet<u8>;