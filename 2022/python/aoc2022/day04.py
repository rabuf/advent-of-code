import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    elves = line.split(",")
    a, b = elves[0].split("-"), elves[1].split("-")
    return tuple(map(int, a)), tuple(map(int, b))


def contains(ranges):
    (a, b), (c, d) = ranges
    return a <= c <= d <= b or c <= a <= b <= d


def overlaps(ranges):
    (a, b), (c, d) = ranges
    return a <= d and c <= b


def main():
    input_path = Path(sys.argv[1])
    with open(input_path / "04.txt", "r") as f:
        lines = f.readlines()
        ranges = list(map(parse_line, lines))
        a = sum(map(contains, ranges))
        b = sum(map(overlaps, ranges))
        print_day(4, a, b)


if __name__ == "__main__":
    main()
