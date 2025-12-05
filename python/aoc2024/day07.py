import sys
from math import floor, log10
from operator import add, floordiv, mul, sub
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    result, operands = line.split(":")
    return int(result), list(int(operand) for operand in operands.split())


def check_inverse_concatenate(x, current):
    return current % 10 ** floor(log10(x) + 1) == x


def check_sub(_x, _current):
    return True


def check_div(x, current):
    return current % x == 0


def reverse_search(
    expected, operands, operations=(sub, floordiv), checks=(check_sub, check_div)
):
    op_checks = list(zip(operations, checks))

    def recur(current, first, *rest):
        if not rest:
            return current == first
        for op, check in op_checks:
            if check(first, current):
                if recur(op(current, first), *rest):
                    return True
        return False

    return recur(expected, *operands)


def find_operands(expected, operands, operations=(mul, add)):
    def recur(first, second, *rest):
        if first > expected:
            return False
        for op in operations:
            temp = op(first, second)
            if rest:
                if recur(temp, *rest):
                    return True
            else:
                if temp == expected:
                    return True
        return False

    return recur(*operands)


def concatenate(a, b):
    return a * 10 ** floor(log10(b) + 1) + b


def inverse_concatenate(a, b):
    return a // 10 ** floor(log10(b) + 1)


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2024" / "07.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        p1 = sum(
            expected
            for (expected, operands) in lines
            if reverse_search(expected, reversed(operands))
        )
        p2 = sum(
            expected
            for (expected, operands) in lines
            if reverse_search(
                expected,
                reversed(operands),
                (sub, floordiv, inverse_concatenate),
                (check_sub, check_div, check_inverse_concatenate),
            )
        )
        print_day(7, p1, p2)


if __name__ == "__main__":
    main()
