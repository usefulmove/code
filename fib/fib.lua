#! lua

function fib(n)
    if n < 2 then
        return n
    else
        return fib(n-1) + fib(n-2)
    end
end

function fibonacci2(n, a, b)
    a = a or 0
    b = b or 1

    if n == 0 then
        return a
    elseif n == 1 then
        return b
    else
        return fibonacci2(n-1, b, a+b)
    end
end

function fib2(n)
    return fibonacci2(n, 0, 1)
end


print(fib2(tonumber(arg[1])))