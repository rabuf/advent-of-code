import sys
from pathlib import Path


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
        print("Day 04:")
        lines = f.readlines()
        ranges = list(map(parse_line, lines))
        a = sum(map(contains, ranges))
        b = sum(map(overlaps, ranges))
        print(f"\tPart 1: {a}")
        print(f"\tPart 2: {b}")


if __name__ == "__main__":
    main()
