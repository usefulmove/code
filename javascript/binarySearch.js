/* binarySearch :: number -> [number] -> number */
const search = (nums, target) => {
  let [low, high] = [0, nums.length - 1];
  let mid;

  while (low <= high) {
    mid = low + ((high - low) >> 1);

    if (nums[mid] === target) {
      return mid;
    }

    nums[mid] > target ? (high = mid - 1) : (low = mid + 1);
  }

  return -1;
};
