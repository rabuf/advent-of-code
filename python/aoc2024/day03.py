import re
import sys
from itertools import chain
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return re.findall(r"mul\(\d+,\d+\)|do\(\)|don't\(\)", line)


def part1(operations):
    result = 0
    for operation in operations:
        match re.split(r"\(|\)|,", operation):
            case ["mul", lhs, rhs, *_]:
                result += int(lhs) * int(rhs)
    return result


def part2(operations):
    result = 0
    enabled = True
    for operation in operations:
        match re.split(r"\(|\)|,", operation):
            case ["mul", lhs, rhs, *_]:
                result += enabled * int(lhs) * int(rhs)
            case ["do", *_]:
                enabled = True
            case ["don't", *_]:
                enabled = False
    return result


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "03.txt") as f:
        lines = list(chain(*list(map(parse_line, f.read().splitlines()))))
        p1 = part1(lines)
        p2 = part2(lines)
        print_day(1, p1, p2)


if __name__ == '__main__':
    main()
