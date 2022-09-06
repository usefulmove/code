use std::collections::HashSet;
use std::{env, fmt};

fn main() {
    let mut sud: Sudoku = Sudoku::new();

    let mut args: Vec<String> = env::args().collect();
    args.remove(0);

    //println!("debug..args..{:?}", args);

    let mut level: usize = 0;
    args.iter().enumerate().for_each(|(j, s)| {
        if s == "_" {
            sud.board[level][j % 9] = 0;
        } else {
            sud.board[level][j % 9] = s.parse::<u8>().unwrap();
        }
        if j % 9 == 8 { level += 1; }
    });

    println!(
        " input board:\n\n{}",
        sud,
    );

    while !(sud.is_solved() || sud.is_stale()) {
        let old_board = sud.board.clone();

        for i in 0..9 {
            for j in 0..9 {
                if sud.board[i][j] == 0 {
                    match sud.solve_location(i, j) {
                        Some(value) => {
                            sud.board[i][j] = value;
                        }
                        None => (), // do nothing
                    }
                }
            }
        }

        if sud.is_equal(&old_board) { sud.stale = true; }
    }

    println!(
        "\n output board:\n\n{}",
        sud,
    );

    println!(
        "{}",
        match sud.is_stale() {
            true => "err: failed to solve",
            false => "solution found!",
        }
    )
}

struct Sudoku {
    board: [[u8; 9]; 9],
    stale: bool,
}

#[derive(PartialEq, Eq)]
enum Group {
    A, B, C, D, E, F, G, H, I
}

impl Sudoku {
    fn new() -> Self {
        Self { board: [[0; 9]; 9], stale: false }
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

    fn is_stale(&self) -> bool {
        self.stale
    }

    fn is_equal(&self, board_b: &[[u8; 9]; 9]) -> bool {
        for i in 0..9 {
            for j in 0..9 {
                if self.board[i][j] != board_b[i][j] {
                    return false;
                }
            }
        }
        true
    }

    fn solve_location(&self, a: usize, b: usize) -> Option<u8> {
        let mut column_set: Set = Set::new();
        let mut row_set: Set = Set::new();
        let mut group_set: Set = Set::new();

        // build sets
        for i in 0..9 {
            for j in 0..9 {
                // ignore empty cells
                if self.board[i][j] == 0 { continue; }

                // check column
                if j == b {
                    column_set.insert(self.board[i][j]);
                }

                // check row
                if i == a {
                    row_set.insert(self.board[i][j]);
                }

                // check group
                if self.get_group(i, j) == self.get_group(a, b) {
                    group_set.insert(self.board[i][j]);
                }
            }
        }

        // check for solution ( set comparison )
        let complete_set: Set = Set::from([1, 2, 3, 4, 5, 6, 7, 8, 9]);
        let check_set: Set = column_set
            .union(&row_set)
            .cloned()
            .collect::<Set>()
            .union(&group_set)
            .cloned()
            .collect::<Set>();

        match check_set.len() {
            8 => Some(complete_set.difference(&check_set).collect::<Vec<&u8>>()[0].clone()),
            _ => None,
        }
    }

    fn get_group(&self, a: usize, b: usize) -> Group {
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
            write!(
                f,
                " {}",
                self.board[level][n % 9],
            ).unwrap();
            if n % 9 == 8 {
                write!(
                    f,
                    "\n",
                ).unwrap();
                level += 1;
            }

        }
        Ok(())
    }
}

type Set = HashSet<u8>;