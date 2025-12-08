import sys
from functools import cache
from pathlib import Path

from frozendict import frozendict

from aoc_util import print_day

YEAR = "2025"
DAY = "07"


def to_grid(lines):
    grid = {}
    start = 0
    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            grid[complex(i, j)] = c
            if c == "S":
                start = complex(i, j)
    return grid, start


def part1(grid, start):
    splits = 0
    beams = {start}
    while beams:
        next_beams = set()
        for b in beams:
            n = b + 1j
            if n in grid and grid[n] == ".":
                grid[n] = "|"
                next_beams.add(n)
            if n in grid and grid[n] == "^":
                if grid[n - 1] == ".":
                    grid[n - 1] = "|"
                    next_beams.add(n - 1)
                if grid[n + 1] == ".":
                    grid[n + 1] = "|"
                    next_beams.add(n + 1)
                splits += 1
        beams = next_beams
    return splits


@cache
def dfs(grid, pos):
    n = pos + 1j
    if n not in grid:
        return 1
    if grid[n] == ".":
        return dfs(grid, n)
    if grid[n] == "^":
        total = 0
        if grid[n - 1] == ".":
            total += dfs(grid, n - 1)
        if grid[n + 1] == ".":
            total += dfs(grid, n + 1)
        return total


def part2(grid, start):
    return dfs(frozendict(grid), start)


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / YEAR / f"{DAY}.txt") as f:
            grid, start = to_grid(f.read().splitlines())
        print_day(DAY, part1({**grid}, start), part2(grid, start))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
