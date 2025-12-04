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


def to_remove(grid):
    return {pos for pos in grid if is_removeable(grid, pos)}


def part1(grid):
    return len(to_remove(grid))


def part2(grid):
    count = 0
    while removeable := to_remove(grid):
        count += len(removeable)
        grid = grid - removeable
    return count


def fixpoint(f, *args, **kwargs):
    while (next := f(*args)) != args:
        args = next
    return args


def p3(grid, count):
    removeable = to_remove(grid)
    return grid - removeable, count + len(removeable)


def part3(grid):
    _, count = fixpoint(p3, grid, 0)
    return count


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2025" / "04.txt") as f:
            grid = make_grid(f.read().splitlines())
        print_day("04", part1(grid), part2(grid), part3(grid))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
