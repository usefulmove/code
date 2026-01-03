#include <stdio.h>

int fib(int n) {
  if (n < 2) {
    return n;
  } else {
    return fib(n-1) + fib(n-2);
  }
}

int fib2(int n) {
  return n < 2 ? n : fib2(n-1) + fib2(n-2);
}

int main() {
    printf("%d\n", fib2(38));
    return 0;
}
