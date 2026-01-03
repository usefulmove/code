#include <iostream>

int fib(int n) {
    if (n < 2) { return n; }

    return fib(n - 1) + fib(n - 2);
}

int main(int argc, char** argv) {
    if (argc < 2) {
        std::cout << "  error: no argument.\n";
        return -1;
    }

    int arg = std::__cxx11::stoi(argv[1]);

    std::cout << fib(arg) << "\n";
}
