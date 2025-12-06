import sys
from functools import reduce
from operator import add, mul
from pathlib import Path

from aoc_util import print_day

YEAR = "2025"
DAY = "06"


def parse_line(line):
    return line.split()


def part1(problems):
    total = 0
    for problem in problems:
        entries = [int(entry) for entry in problem[1:]]
        if problem[0] == "+":
            total += sum(int(entry) for entry in entries)
        if problem[0] == "*":
            total += reduce(mul, entries)
    return total


def part2(problems):
    total = 0
    transposed = list(zip(*problems))
    transposed = ["".join(element).lstrip() for element in transposed]
    entries = []
    op = None
    for row in transposed:
        if row == "":
            total += reduce(op, entries)
            entries = []
            op = None
            continue
        if not op:
            op = mul if row[-1] == "*" else add
            val = int(row[:-1])
            entries.append(val)
        else:
            entries.append(int(row))
    if op:
        total += reduce(op, entries)
    return total


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / YEAR / f"{DAY}.txt") as f:
            raw = f.read().splitlines()
            lines = list(map(parse_line, raw))
        problems = [list(reversed(line)) for line in zip(*lines)]
        print_day(DAY, part1(problems), part2(raw))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
