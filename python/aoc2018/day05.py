import string
import sys
from pathlib import Path

from aoc_util import print_day


def reactp(a: chr, b: chr) -> bool:
    return a.lower() == b.lower() and a != b


def part1(line):
    stack = []
    for c in line:
        stack.append(c)
        while len(stack) >= 2 and reactp(stack[-1], stack[-2]):
            stack.pop()
            stack.pop()
    return len(stack)


def part2(line):
    return min(
        part1(line.replace(c, "").replace(c.upper(), ""))
        for c in string.ascii_lowercase
    )


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2018" / "05.txt") as f:
        line = f.readline().strip()
        print_day(1, part1(line), part2(line))


if __name__ == "__main__":
    main()
