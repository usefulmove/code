from collections import deque
from functools import reduce
from toolz import curry
from typing import Tuple


# evaluateOps :: Tuple[str] -> deque -> deque
@curry
def evaluateOps(ops: Tuple[str], indeq: deque) -> deque:
    def processOp(indeq: deque, op: str) -> deque:
        """
        search command dictionary for command function to execute on
        input deque - otherwise, add (value) to deque
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
