import sys
from functools import partial
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


def neighbor_count(grid, pos):
    return sum(pos + d in grid for d in OFFSETS)


def is_removeable(grid, pos):
    return neighbor_count(grid, pos) < 4


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


def part4(grid):
    with_counts = {pos: neighbor_count(grid, pos) for pos in grid}
    removeable = to_remove(grid)
    count = len(removeable)
    for r in removeable:
        del with_counts[r]
    while removeable:
        r = removeable.pop()
        for n in (r + d for d in OFFSETS):
            if n in with_counts:
                with_counts[n] = with_counts[n] - 1
                if with_counts[n] < 4:
                    removeable.add(n)
                    del with_counts[n]
                    count = count + 1

    return count


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2025" / "04.txt") as f:
            grid = make_grid(f.read().splitlines())
        print_day("04", part4(grid))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
