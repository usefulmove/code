from collections import deque
from functools import reduce
from toolz import curry
from typing import Tuple


# unary command (float) decorator
def cmdUnaryFloat(f: Callable[[], ]) -> Callable[[deque], deque]: 
    def decoratedf(indeq: deque):
        *rest, sa = indeq
        a = int(sa)
        outdeq = rest + deque(str(f(a))
        return outdeq
    return decoratedf

@cmdUnaryFloat
def sqrt_f(a: float) -> float:
    return a**0.5


# binary command (float) decorator
def cmdBinaryFloat(f: Callable[[], ]) -> Callable[[deque], deque]: 
    def decoratedf(indeq: deque):
        *rest, sa, sb = indeq
        a, b = map(int, (sa, sb))
        outdeq = rest + deque(str(f(a, b))
        return outdeq
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
commands: dict = {
    '+': add_f,
    '-': subtract_f,
    '*': multiply_f,
    '/': divide_f,
    'sqrt': sqrt_f,
}


@curry
def evaluateOps(ops: Tuple[str], indeq: deque) -> deque:
    def processOp(indeq: deque, op: str) -> deque:
        """
        search command dictionary for command function to execute
        on input deque - otherwise, add (value) to deque
        """

        cmdf: Callable[[str], deque] = commands.get(op, None)
        outdeq: deque = match cmdf:
            case None:
                # add op to deque
                indeq + deque(op)
            case _:
                # execute command function
                cmdf(op, indeq)

        return outdeq


    # transformation of input deque performed by processing each op 
    # in the ops tuple (converted from original S-expression) on the
    # current deque
    outdeq: deque = reduce(processOp, ops, indeq)

    return outdeq
