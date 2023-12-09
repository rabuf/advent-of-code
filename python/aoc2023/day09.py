from operator import sub
import sys
from pathlib import Path

from aoc_util import print_day


def extrapolate(row):
    if all(n == 0 for n in row):
        return 0
    return extrapolate(forward_difference(row)) + row[-1]


def extrapolate_backwards(row):
    if all(n == 0 for n in row):
        return 0
    return row[0] - extrapolate_backwards(forward_difference(row))


def forward_difference(row):
    return list(map(sub, row[1:], row))


def parse_line(line):
    return [int(n) for n in line.split()]


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "09.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        extrapolations = [extrapolate(line) for line in lines]
        backward_extrapolations = [extrapolate_backwards(line) for line in lines]
        print_day(9, sum(extrapolations), sum(backward_extrapolations))


if __name__ == '__main__':
    main()
