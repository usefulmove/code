function getPermutations(arr) {
  const results = [];

  function permute(arr, memo = []) {
    if (arr.length === 0) {
      results.push(memo);
    } else {
      for (let i = 0; i < arr.length; i++) {
        let curr = arr.slice();
        let next = curr.splice(i, 1);
        permute(curr.slice(), memo.concat(next));
      }
    }
  }

  permute(arr);
  return results;
}

const numbers = [0, 1, 2, 3, 4, 5];
const permutations = getPermutations(numbers);

console.log(permutations);
