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


def tilt(row, reverse=False):
    return list('#'.join(''.join(sorted(section, reverse=reverse)) for section in (''.join(row)).split('#')))


def tilt_east(grid):
    grid[:] = [tilt(row) for row in grid]


def tilt_south(grid):
    grid[:] = [[*row] for row in transpose(grid)]
    tilt_east(grid)
    grid[:] = [[*row] for row in transpose(grid)]


def tilt_west(grid):
    grid[:] = [tilt(row, reverse=True) for row in grid]


def tilt_north(grid):
    grid[:] = [[*row] for row in transpose(grid)]
    tilt_west(grid)
    grid[:] = [[*row] for row in transpose(grid)]


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
        count = mu + (1000000000 - mu) % lam
        p2 = load(spin_cycle(lines, times=count))
        print_day(14, load(tilted), p2)


if __name__ == '__main__':
    main()
