package main
import "fmt"

func main() {
	fmt.Println(fib2(10))
}

func fibonacci2(n int, a int, b int) int {
	if (n == 0) {
		return a
	} else if (n == 1) {
		return b
	} else {
		return fibonacci2(n-1, b, a+b)
	}
}

func fib2(n int) int {
	return fibonacci2(n, 0, 1)
}