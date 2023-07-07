from pyrsistent import pdeque, PDeque
from functools import reduce
import math
from toolz import curry
from typing import Callable, Tuple, Dict


# unary command (float) decorator
def cmdUnaryFloat(f: Callable[[float], float]) -> Callable[[PDeque[str]], PDeque[str]]:
    def decoratedf(indeq: PDeque[str]):
        *rest, sa = indeq
        a = float(sa)
        return pdeque(rest).append(str(f(a)))

    return decoratedf


@cmdUnaryFloat
def sqrt_f(a: float) -> float:
    return math.sqrt(a)


# binary command (float) decorator
def cmdBinaryFloat(
    f: Callable[[float, float], float]
) -> Callable[[PDeque[str]], PDeque[str]]:
    def decoratedf(indeq: PDeque[str]):
        *rest, sa, sb = indeq
        a, b = map(float, (sa, sb))
        return pdeque(rest).append(str(f(a, b)))

    return decoratedf


@cmdBinaryFloat
def add_f(a: float, b: float) -> float:
    return a + b


@cmdBinaryFloat
def subtract_f(a: float, b: float) -> float:
    return a - b


@cmdBinaryFloat
def multiply_f(a: float, b: float) -> float:
    return a * b


@cmdBinaryFloat
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
def evaluateOps(ops: Tuple[str], indeq: PDeque[str]) -> PDeque[str]:
    def processOp(indeq: PDeque[str], op: str) -> PDeque[str]:
        """
        search command dictionary for command function to execute
        on input deque - otherwise, add (value) to deque
        """
        cmdf: Callable[[PDeque[str]], PDeque[str]] = commands.get(op, lambda d: d)
        return indeq.append(op) if cmdf is None else cmdf(indeq)

    # transformation of input deque performed by processing each op
    # in the ops tuple (converted from original S-expression) on the
    # current deque
    return reduce(processOp, ops, indeq)
