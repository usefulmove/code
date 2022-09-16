struct Solution;

impl Solution {
    pub fn restore_matrix(row_sum: &Vec<i32>, col_sum: &Vec<i32>) -> Vec<Vec<i32>> {
        for i in 0..28 {
            // TODO
        }
        vec![vec![0; 2]; 2]
    }

    pub fn is_solved(matrix: &Vec<Vec<i32>>, row_sum: &Vec<i32>, col_sum: &Vec<i32>) -> bool {
        println!("debug..matrix[0].len()={}", matrix[0].len());
        /* row check */
        for (i, row) in matrix.iter().cloned().enumerate() {
            let sum: i32 = row.iter().sum();
            if sum != row_sum[i] { return false }
        }

        /* column check */
        for j in 0..matrix[0].len() {
            let sum: i32 = matrix.iter().map(|row| row[j]).sum();
            if sum != col_sum[j] { return false }
        }

        true
    }
}

fn main() {
    let row_sum = vec![3, 8];
    let col_sum = vec![4, 7];
    let ren = Solution::restore_matrix(&row_sum, &col_sum);
    println!("{:?}", ren);
    println!("solved:{}", Solution::is_solved(&ren, &row_sum, &col_sum));
}
