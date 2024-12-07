import sys
from operator import mul, add
from pathlib import Path
from math import log10, floor

from aoc_util import print_day


def parse_line(line):
    result, operands = line.split(':')
    return int(result), list(int(operand) for operand in operands.split())


def find_operands(expected, operands, operations=(mul, add)):
    def recur(first, second, *rest):
        if first > expected: return False
        result = False
        for op in operations:
            temp = op(first, second)
            if rest:
                result = result or recur(temp, *rest)
            else:
                result = result or temp == expected
        return result

    return recur(*operands)


catenate = lambda a, b: a * 10 ** floor(log10(b) + 1) + b


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "07.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        p1 = sum(expected for (expected, operands) in lines if find_operands(expected, operands))
        p2 = sum(expected for (expected, operands) in lines if find_operands(expected, operands, (mul, add, catenate)))
        print_day(7, p1, p2)


if __name__ == '__main__':
    main()
