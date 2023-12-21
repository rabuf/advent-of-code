import sys
from collections import defaultdict
from pathlib import Path

from aoc_util import print_day


def lines_to_grid(lines):
    grid = defaultdict(str)
    start = 0
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            pos = x + y * 1j
            grid[pos] = c
            if c == 'S':
                start = pos
                grid[pos] = '.'
    return grid, start


def gardens_within(grid, start, steps):
    places = {start}
    for steps in range(steps):
        next_places = set()
        for place in places:
            for delta in [1j, -1j, 1, -1]:
                if grid[place+delta] == '.':
                    next_places.add(place + delta)
        places = next_places
    return sum(grid[place] == '.' for place in places)


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "21.txt") as f:
        lines = f.read().splitlines()
        grid, start = lines_to_grid(lines)
        gardens = gardens_within(grid, start, 64)
        print_day(21, gardens, len(lines))


if __name__ == '__main__':
    main()
