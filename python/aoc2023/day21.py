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


def infinite_gardens(lines, start, steps, display=False):
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
        if display:
            print(f'Step {steps}: {len(places)}')
    return gardens


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "21.txt") as f:
        lines = f.read().splitlines()
        grid, start = lines_to_grid(lines)
        gardens = gardens_within(grid, start, 64)
        p2 = infinite_gardens(lines, start, 400, display=False)
        d1 = [*map(sub, p2[1:], p2)]
        d2 = [*map(sub, d1[1:], d1)]

        print_day(21, gardens, p2)


if __name__ == '__main__':
    main()
