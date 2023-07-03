from collections import deque
from functools import reduce
from toolz import curry
from typing import Tuple


# built-in commands
commands: dict = {
    '+': add_f,
    '-': subtract_f,
}


# unary command (float) decorator
def cmdUnaryFloat(f: Callable[[], ]) -> Callable[[deque], deque]: 
    def decoratedf(indeq: deque):
        *rest, sa = indeq
        a = int(sa)
        outdeq = rest + deque(str(f(a))
        return outdeq
    return decoratedf

@cmdUnaryFloat
def add_f(a: float) -> float:
    return f(a)


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
