import sys
from pathlib import Path
from collections import Counter

from aoc_util import print_day


def parse_line(line: str):
    l, r = line.split()
    return int(l), int(r)


def part1(left, right):
    left = sorted(left)
    right = sorted(right)
    return sum(abs(l - r) for (l,r) in zip(left, right))


def part2(left, right):
    counts = Counter(right)
    return sum(l * counts[l] for l in left)

def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "01.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        p1 = part1(*zip(*lines))
        p2 = part2(*zip(*lines))
        print_day(1, p1, p2)


if __name__ == '__main__':
    main()
