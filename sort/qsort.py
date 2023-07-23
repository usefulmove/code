from typing import Sequence


# qsorted - return a new sorted sequence
# qsorted :: Sequence -> Sequence
def qsorted(orig: Sequence) -> Sequence:
    seq = orig[:]

    if len(seq) <= 1:
        return seq

    # choose pivot
    pivot = seq[0]

    # move pivot to end
    seq[0], seq[-1] = seq[-1], seq[0]

    ins = 0  # insertion index
    for i in range(len(seq) - 1):
        if seq[i] < pivot:
            # swap with insertion index and increment insertion index
            seq[ins], seq[i] = seq[i], seq[ins]
            ins += 1

    # swap pivot with insertion index
    seq[ins], seq[-1] = seq[-1], seq[ins]

    # recursively sort left and right sides
    return qsorted(seq[:ins]) + [pivot] + qsorted(seq[ins + 1:])


# qsort - sort in place
# qsort :: Sequence -> None
def qsort(seq: Sequence, start=0, end=None) -> None:
    if not end:
        end = len(seq) - 1

    if end - start <= 1:
        return

    # choose pivot
    pivot = seq[start]

    # move pivot to end
    seq[start], seq[end] = seq[end], seq[start]

    ins = start  # insertion index
    for i in range(start, end):
        if seq[i] < pivot:
            # swap with insertion index and increment insertion index
            seq[ins], seq[i] = seq[i], seq[ins]
            ins += 1

    # swap pivot with insertion index
    seq[ins], seq[end] = seq[end], seq[ins]

    # recursively sort left and right sides
    qsort(seq, start, ins)
    qsort(seq, ins + 1, end)
