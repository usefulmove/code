from pyrsistent import pdeque
from functools import reduce
import math
from toolz import curry
from typing import Callable, Tuple, Dict


# unary command (float) decorator
def commandUnaryFloat(f: Callable[[float], float]) -> Callable[[pdeque], pdeque]:

    def decoratedf(indeq: pdeque):
        *rest, sa = indeq
        a = float(sa)
        return pdeque(rest).append(str(f(a)))

    return decoratedf


@commandUnaryFloat
def sqrt_f(a: float) -> float:
    return math.sqrt(a)


# binary command (float) decorator
def commandBinaryFloat(f: Callable[[float, float], float]) -> Callable[[pdeque], pdeque]:

    def decoratedf(indeq: pdeque):
        *rest, sa, sb = indeq
        a, b = map(float, (sa, sb))
        return pdeque(rest).append(str(f(a, b)))

    return decoratedf


@commandBinaryFloat
def add_f(a: float, b: float) -> float:
    return a + b


@commandBinaryFloat
def subtract_f(a: float, b: float) -> float:
    return a - b


@commandBinaryFloat
def multiply_f(a: float, b: float) -> float:
    return a * b


@commandBinaryFloat
def divide_f(a: float, b: float) -> float:
    return a / b


# built-in commands
commands: Dict[str, Callable] = {
    "+": add_f,
    "-": subtract_f,
    "*": multiply_f,
    "/": divide_f,
    "sqrt": sqrt_f,
}


@curry
def evaluateOps(ops: Tuple[str], indeq: pdeque) -> pdeque:
    def processOp(indeq: pdeque, op: str) -> pdeque:
        """
        search command dictionary for command function to execute
        on input deque - otherwise, add (value) to deque
        """
        cmdf: Callable[[pdeque], pdeque] = commands.get(op, None)

        return indeq.append(op) if cmdf is None else cmdf(indeq)

    # transformation of input deque performed by processing each op
    # in the ops tuple (converted from original S-expression) on the
    # current deque
    return reduce(processOp, ops, indeq)


def main() -> None:
    input_deque: pdeque = pdeque()  # input deque

    # S-expression
    s_expression = "5 sqrt 1 - 2 /"

    # transform input deque by evaluating operations contained in S-expression
    ops = tuple(s_expression.split())
    output_deque: pdeque = evaluateOps(ops, input_deque)  # output deque

    print(output_deque)


if __name__ == "__main__":
    main()