import re
import sys
from pathlib import Path

import z3
from shapely import LineString, box, Point

from aoc_util import print_day


def parse_line(line):
    return [int(n) for n in re.findall(r'-?\d+', line)]


def shift_stone(stone, lower=7, upper=27):
    [x, y, z, dx, dy, dz] = stone
    m = dy / dx
    b = y - x * m
    left = Point((lower, m * lower + b))
    right = Point((upper, m * upper + b))
    return LineString([left, right])


def sign(n):
    if n < 0:
        return -1
    if n > 0:
        return 1
    return 0


def solve_p2_z3(hailstones):
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


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "24.txt") as f:
        hailstones = list(map(parse_line, f.read().splitlines()))
        lower, upper = 200000000000000, 400000000000000
        region = box(lower, lower, upper, upper)
        regionalized = [shift_stone(stone, lower=lower, upper=upper) for stone in hailstones]
        count = 0
        for i, s1 in enumerate(regionalized[:-1]):
            for j, s2 in enumerate(regionalized[i + 1:], start=i + 1):
                intersection = s1.intersection(s2)
                intersection = intersection.intersection(region)
                if intersection:
                    if (sign(intersection.x - hailstones[i][0]) == sign(hailstones[i][3])
                            and sign(intersection.x - hailstones[j][0]) == sign(hailstones[j][3])
                            and sign(intersection.y - hailstones[i][1]) == sign(hailstones[i][4])
                            and sign(intersection.y - hailstones[j][1]) == sign(hailstones[j][4])):
                        count += 1
        p2 = solve_p2_z3(hailstones)
        print_day(24, count, p2)


if __name__ == '__main__':
    main()
