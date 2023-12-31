import re
import sys
from itertools import combinations
from pathlib import Path

import z3
from shapely import LineString, box, Point

from aoc_util import print_day


def parse_line(line):
    return [int(n) for n in re.findall(r'-?\d+', line)]


def extend_line(stone, lower=7, upper=27):
    [x, y, z, dx, dy, dz] = stone
    origin = Point((x, y))
    distance = 0
    if dx > 0:
        distance = (upper - x) / dx
    if dx < 0:
        distance = (lower - x) / dx
    return LineString([origin, Point((x + distance * dx, y + distance * dy))])


def solve_p2(hailstones):
    [x, y, z, dx, dy, dz] = [z3.Int(name) for name in ['x', 'y', 'z', 'dx', 'dy', 'dz']]
    times = z3.IntVector("times", len(hailstones))
    s = z3.Solver()
    for t, [hx, hy, hz, hdx, hdy, hdz] in zip(times, hailstones):
        s.add(x + t * dx == hx + t * hdx)
        s.add(y + t * dy == hy + t * hdy)
        s.add(z + t * dz == hz + t * hdz)
    s.check()
    m = s.model()
    return m[x].as_long() + m[y].as_long() + m[z].as_long()


def solve_p1(hailstones, lower, upper):
    region = box(lower, lower, upper, upper)
    extended = [extend_line(stone, lower=lower, upper=upper) & region for stone in hailstones]
    return sum(bool(s1 & s2) for s1, s2 in combinations(extended, 2))


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "24.txt") as f:
        hailstones = list(map(parse_line, f.read().splitlines()))
        # lower, upper = 7, 27
        lower, upper = 200000000000000, 400000000000000
        p1 = solve_p1(hailstones, lower, upper)
        p2 = solve_p2(hailstones)
        print_day(24, p1, p2)


if __name__ == '__main__':
    main()
