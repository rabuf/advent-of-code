import sys
from pathlib import Path

from aoc_util import print_day


def make_grid(lines):
    grid = set()
    for row, line in enumerate(lines):
        for column, c in enumerate(line):
            if c == "@":
                grid.add(row + 1j * column)
    return grid


OFFSETS = (1j, -1j, 1, -1, 1 + 1j, 1 - 1j, -1 + 1j, -1 - 1j)


def is_removeable(grid, pos):
    return sum(pos + d in grid for d in OFFSETS) < 4


def part1(grid):
    return sum(is_removeable(grid, pos) for pos in grid)


def to_remove(grid):
    return {pos for pos in grid if is_removeable(grid, pos)}


def part2(grid):
    count = 0
    while removeable := to_remove(grid):
        count += len(removeable)
        grid = grid - removeable
    return count


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2025" / "04.txt") as f:
            grid = make_grid(f.read().splitlines())
        print_day("04", part1(grid), part2({*grid}))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
