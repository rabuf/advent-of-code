import bisect
import sys
from collections import defaultdict
from itertools import cycle
from math import atan2
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return line


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "10.txt") as f:
            lines = map(parse_line, f.read().splitlines())
        grid = {}
        for j, line in enumerate(lines):
            for i, c in enumerate(line):
                if c == '#':
                    grid[(i, j)] = 0
        for (ax, ay) in grid:
            visible = defaultdict(list)
            for (ox, oy) in grid:
                if (ox, oy) == (ax, ay): continue
                sx, sy = ax - ox, ay - oy
                visible[atan2(sy, sx)].append((ox, oy))
            grid[(ax, ay)] = visible
        best, visible = max(grid.items(), key=lambda a: len(a[1]))
        p1 = len(visible)

        def distance(asteroid):
            return abs(best[0] - asteroid[0]) + abs(best[1] - asteroid[1])

        for k, v in visible.items():
            visible[k] = list(sorted(v, key=distance))

        i = 0
        slopes = list(sorted(visible))
        start = bisect.bisect_left(slopes, atan2(1, 0))
        for slope in cycle(slopes[start:] + slopes[:start]):
            if visible[slope]:
                ax, ay = visible[slope].pop()
                i = i + 1
                if i == 200: break

        p2 = 100 * ax + ay
        print_day("10", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
