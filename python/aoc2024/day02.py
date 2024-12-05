import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return [int(i) for i in line.split()]


def safe(record):
    differences = [a - b for a, b in zip(record, record[1:])]
    return ((all(n > 0 for n in differences) or all(n < 0 for n in differences))
            and all(1 <= abs(n) <= 3 for n in differences))


def make_safe(record):
    return any(safe(record[0:i] + record[i + 1:]) for i in range(len(record)))


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "02.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        p1 = sum(map(safe, lines))
        p2 = sum(map(make_safe, lines))
        print_day(2, p1, p2)


if __name__ == '__main__':
    main()
