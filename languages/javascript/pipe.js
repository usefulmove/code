const pipe = (...fns) => (a) => fns.reduce((acc, fn) => fn(acc), a);

const square = (a) => a * a;
const double = (a) => a * 2;

console.log(pipe(square, double, double, double)(8));
