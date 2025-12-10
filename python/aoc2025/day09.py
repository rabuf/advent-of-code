import sys
from itertools import combinations
from pathlib import Path

from shapely import Polygon

from aoc_util import print_day

YEAR = "2025"
DAY = "09"


def parse_line(line):
    return tuple(map(int, line.split(",")))


def area(a, b):
    ax, ay = a
    bx, by = b
    return (1 + abs(ax - bx)) * (1 + abs(ay - by))


def part1(squares):
    best = 0
    for i, a in enumerate(squares):
        for b in squares[i + 1 :]:
            best = max(area(a, b), best)
    return best


SAMPLE = """7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"""


def part2(points):
    polygon = Polygon(points)
    for a, b in sorted(combinations(points, 2), key=lambda a: area(*a), reverse=True):
        c = (a[0], b[1])
        d = (b[0], a[1])
        rectangle = Polygon([a, c, b, d])
        if polygon.contains(rectangle):
            return area(a, b)


def main(input_dir=Path(sys.argv[1])):
    try:
        sample = list(map(parse_line, SAMPLE.splitlines()))
        with open(input_dir / YEAR / f"{DAY}.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        print_day(DAY, part1(lines), part2(lines))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
