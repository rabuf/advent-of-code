import sys
from pathlib import Path

from more_itertools import transpose

import aoc_util
from aoc_util import print_day


def load(grid):
    height = len(grid)
    weight_load = 0
    for y, row in enumerate(grid):
        weight_load += (height - y) * row.count('O')
    return weight_load


def tilt(row):
    '#'.join(''.join(section) for section in row.split('#'))


def tilt_east(grid):
    changed = True
    while changed:
        changed = False
        for y, row in enumerate(grid):
            for x in range(len(row)-1, 0, -1):
                if row[x] == '.' and row[x-1] == 'O':
                    row[x], row[x-1] = row[x-1], row[x]
                    changed = True


def tilt_south(grid):
    changed = True
    while changed:
        changed = False
        for y in range(len(grid) - 1, 0, -1):
            for x, cell in enumerate(grid[y]):
                if cell == '.' and grid[y-1][x] == 'O':
                    grid[y-1][x], grid[y][x] = grid[y][x], grid[y-1][x]
                    changed = True


def tilt_west(grid):
    changed = True
    while changed:
        changed = False
        for y, row in enumerate(grid):
            for x in range(len(row) - 1):
                if row[x] == '.' and row[x+1] == 'O':
                    row[x], row[x+1] = row[x+1], row[x]
                    changed = True


def tilt_north(grid):
    changed = True
    while changed:
        changed = False
        for y in range(len(grid) - 1):
            for x in range(len(grid[y])):
                if grid[y][x] == '.' and grid[y+1][x] == 'O':
                    grid[y][x], grid[y+1][x] = 'O', '.'
                    changed = True


def print_grid(grid):
    print('\n'.join(''.join(row) for row in grid))
    print()


def cycle_finder(grid):
    power = lam = 1
    tortoise = [row.copy() for row in grid]
    hare = spin_cycle([row.copy() for row in grid])
    while tortoise != hare:
        if power == lam:
            tortoise = [row.copy() for row in hare]
            power *= 2
            lam = 0
        spin_cycle(hare)
        lam += 1
    tortoise = [row.copy() for row in grid]
    hare = [row.copy() for row in grid]
    spin_cycle(hare, times=lam)

    mu = 0
    while tortoise != hare:
        spin_cycle(tortoise)
        spin_cycle(hare)
        mu += 1
    return lam, mu


def spin_cycle(grid, times=1):
    for i in range(times):
        tilt_north(grid)
        tilt_west(grid)
        tilt_south(grid)
        tilt_east(grid)
    return grid


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "14.txt") as f:
        lines = [list(line) for line in f.read().splitlines()]
        tilted = [row.copy() for row in lines]
        tilt_north(tilted)
        grid = [row.copy() for row in lines]
        lam, mu = cycle_finder(grid)
        print((lam, mu))
        count = mu + (1000000000 - mu) % lam
        p2 = load(spin_cycle(lines, times=count))
        print_day(14, load(tilted), p2)


if __name__ == '__main__':
    main()
