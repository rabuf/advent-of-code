import sys
from operator import sub
from pathlib import Path

from aoc_util import print_day


def lines_to_grid(lines):
    grid = {}
    start = 0
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            pos = (x, y)
            grid[pos] = c
            if c == 'S':
                start = pos
                grid[pos] = '.'
    return grid, start


def gardens_within(grid, start, steps):
    places = {start}
    for steps in range(steps):
        next_places = set()
        for x, y in places:
            for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
                if (dx + x, dy + y) in grid and grid[(dx + x, dy + y)] == '.':
                    next_places.add((dx + x, dy + y))
        places = next_places
    return len(places)


def infinite_gardens(lines, start, steps):
    height, width = len(lines), len(lines[0])
    places = {start}
    gardens = [1]
    for steps in range(steps):
        next_places = set()
        for x, y in places:
            for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
                if lines[(y + dy) % height][(x + dx) % width] in '.S':
                    next_places.add((dx + x, dy + y))
        places = next_places
        gardens.append(len(places))
    return gardens


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "21.txt") as f:
        lines = f.read().splitlines()
        grid, start = lines_to_grid(lines)
        gardens = gardens_within(grid, start, 64)
        # Magic Numbers: 65, 131; the target is 26501365 which is 65 + 131 * 202300
        # 131 is the width/height of each section
        # The area (or potential) grows quadratically since it's a diamond, idea is to fit
        # a quadratic equation and plug in 202300

        p2 = infinite_gardens(lines, start, 65+131*2)
        # key numbers:
        # v0 = c
        # v1 = a + b + c
        # v2 = 4a + 2b + c
        # c = v0
        # a = (v2 - 2v1 + v0) / 2
        # b = v1 - c - a
        v = [p2[65], p2[131 + 65], p2[131*2 + 65]]
        c = v[0]
        a = (v[2] - 2 * v[1] + v[0]) // 2
        b = v[1] - a - c
        p = lambda n: a * n * n + b * n + c
        p2 = p(202300)
        print_day(21, gardens, p2)


if __name__ == '__main__':
    main()
