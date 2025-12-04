#!/usr/bin/env sh
fib() {
    local n=$1

    if [ $n -lt 2 ]; then
        echo $n
    else
        res=$(($(fib $(($n-1))) + $(fib $(($n-2)))))
        echo $res
    fi
}
echo $(fib $1)
