import sys
from pathlib import Path

from more_itertools import transpose

from aoc_util import print_day


def load(grid):
    return sum(y * row.count("O") for y, row in enumerate(grid[::-1], start=1))


def tilt(row, reverse=False):
    return "#".join(
        "".join(sorted(section, reverse=reverse)) for section in row.split("#")
    )


def tilt_east(grid):
    grid[:] = [tilt(row) for row in grid]


def tilt_south(grid):
    grid[:] = ["".join(row) for row in transpose(grid)]
    tilt_east(grid)
    grid[:] = ["".join(row) for row in transpose(grid)]


def tilt_west(grid):
    grid[:] = [tilt(row, reverse=True) for row in grid]


def tilt_north(grid):
    grid[:] = ["".join(row) for row in transpose(grid)]
    tilt_west(grid)
    grid[:] = ["".join(row) for row in transpose(grid)]


def print_grid(grid):
    print("\n".join("".join(row) for row in grid))
    print()


def cycle_finder(grid):
    power = lam = 1
    tortoise = grid.copy()
    hare = spin_cycle(grid.copy())
    while tortoise != hare:
        if power == lam:
            tortoise = hare.copy()
            power *= 2
            lam = 0
        spin_cycle(hare)
        lam += 1
    tortoise = grid.copy()
    hare = grid.copy()
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


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "14.txt") as f:
        lines = f.read().splitlines()
        tilted = lines.copy()
        tilt_north(tilted)
        grid = lines.copy()
        lam, mu = cycle_finder(grid)
        count = mu + (1000000000 - mu) % lam
        p2 = load(spin_cycle(lines, times=count))
        print_day(14, load(tilted), p2)


if __name__ == "__main__":
    main()
