const fib = (n: number): number => n < 2 ? n : fib(n-1) + fib(n-2);

const fib2 = (n: number, a: number = 0, b: number = 1): number => {
  switch (n) {
    case 0: return a;
    case 1: return b;
    default: return fib2(n-1, b, a+b);
  }
};

console.log(fib2(38));