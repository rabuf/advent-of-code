import sys
from collections import defaultdict
from functools import reduce
from operator import or_
from pathlib import Path

from aoc_util import print_day


def energized(grid, start=(0 + 0j), direction=(1 + 0j)):
    state = [(direction, start)]
    e = defaultdict(set)
    while state:
        direction, position = state.pop()
        if position not in grid:
            continue
        if position in e[direction]:
            continue
        e[direction].add(position)
        c = grid[position]
        match c:
            case ".":
                state.append((direction, position + direction))
            case "|":
                if direction == 1j or direction == -1j:
                    state.append((direction, position + direction))
                else:
                    state.append((-1j, position - 1j))
                    state.append((1j, position + 1j))
            case "-":
                if direction == 1 or direction == -1:
                    state.append((direction, position + direction))
                else:
                    state.append((-1, position - 1))
                    state.append((1, position + 1))
            case "/":
                if direction == 1 or direction == -1:
                    state.append((direction * -1j, position + direction * -1j))
                else:
                    state.append((direction * 1j, position + direction * 1j))
            case "\\":
                if direction == 1 or direction == -1:
                    state.append((direction * 1j, position + direction * 1j))
                else:
                    state.append((direction * -1j, position + direction * -1j))
    return len(reduce(or_, e.values()))


def lines_to_grid(lines):
    grid = {}
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            grid[x + y * 1j] = c
    return grid


def maximize_energized_region(grid, height, width):
    energy = 0
    for y in range(height):
        energy = max(energy, energized(grid, start=(0 + y * 1j), direction=1))
        energy = max(energy, energized(grid, start=(width - 1 + y * 1j), direction=-1))
    for x in range(width):
        energy = max(energy, energized(grid, start=x, direction=1j))
        energy = max(
            energy, energized(grid, start=(x + (height - 1) * 1j), direction=-1j)
        )
    return energy


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "16.txt") as f:
        lines = f.read().splitlines()
    height, width = len(lines), len(lines[0])
    grid = lines_to_grid(lines)
    energy = energized(grid)
    max_energy = maximize_energized_region(grid, height, width)
    print_day(16, energy, max_energy)


if __name__ == "__main__":
    main()
