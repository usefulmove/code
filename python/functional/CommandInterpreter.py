from pyrsistent import pdeque
from functools import reduce
import math
from operator import add, sub, mul, truediv
from toolz import curry
from typing import Callable as Fn, Tuple, Dict


# unary command (float) decorator
def commandUnaryFloat(f: Fn[[float], float]) -> Fn[[pdeque], pdeque]:

    def decoratedf(indeq: pdeque):
        *rest, sa = indeq
        a = float(sa)
        return pdeque(rest).append(str(f(a)))

    return decoratedf


# binary command (float) decorator
def commandBinaryFloat(f: Fn[[float, float], float]) -> Fn[[pdeque], pdeque]:

    def decoratedf(indeq: pdeque):
        *rest, sa, sb = indeq
        a, b = map(float, (sa, sb))
        return pdeque(rest).append(str(f(a, b)))

    return decoratedf


""" UNARY COMMAND DEFINITIONS """


sqrt_f = commandUnaryFloat(math.sqrt)
inv_f = commandUnaryFloat(lambda a: a**-1)


""" BINARY COMMAND DEFINITIONS """


add_f = commandBinaryFloat(add)
sub_f = commandBinaryFloat(sub)
mul_f = commandBinaryFloat(mul)
div_f = commandBinaryFloat(truediv)


""" STACK COMMAND DEFINITIONS """


def dup_f(indeq: pdeque) -> pdeque:
    return indeq.append(indeq[-1])


# add commands
commands: Dict[str, Fn] = {
    "+": add_f,
    "-": sub_f,
    "*": mul_f,
    "/": div_f,
    "sqrt": sqrt_f,
    "inv": inv_f,
    "dup": dup_f,
}


@curry
def evaluateOps(ops: Tuple[str], indeq: pdeque) -> pdeque:
    def processOp(indeq: pdeque, op: str) -> pdeque:
        """
        search command dictionary for command function to execute
        on input deque - otherwise, add (value) to deque
        """
        cmdf: Fn[[pdeque], pdeque] = commands.get(op, None)

        return indeq.append(op) if not cmdf else cmdf(indeq)

    # transformation of input deque performed by processing each op
    # in the ops tuple (converted from original S-expression) on the
    # current deque
    return reduce(processOp, ops, indeq)


def main() -> None:
    input_deque: pdeque = pdeque()  # input deque

    # S-expression to be evaluated
    s_expression = "5 sqrt 1 - 2 / dup inv"

    # transform input deque by evaluating operations contained in S-expression
    ops = tuple(s_expression.split())
    output_deque: pdeque = evaluateOps(ops, input_deque)  # output deque

    for level in output_deque:
        print(level)


if __name__ == "__main__":
    main()
