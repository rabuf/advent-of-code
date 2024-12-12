import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return line


def lines_to_map(lines):
    grid = {}
    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            grid[i + j * 1j] = c
    return grid


def extract(grid, kind, location):
    frontier = {location}
    squares = set()
    while frontier:
        l = frontier.pop()
        squares.add(l)
        if l in grid: del grid[l]
        neighbors = [l + n for n in (1, -1, 1j, -1j)]
        for n in neighbors:
            if n in grid and grid[n] == kind:
                frontier.add(n)

    return squares


def process_map(grid: dict[complex, str]):
    p1_cost = 0
    p2_cost = 0
    while grid:
        l, kind = grid.popitem()
        squares = extract(grid, kind, l)
        area = len(squares)
        perimeter = area * 4
        on_perimeter = {}
        interior = set()
        for s in squares:
            p = 4
            edges = {1, -1, 1j, -1j}
            for n in (1, -1, 1j, -1j):
                if s + n in squares:
                    p = p - 1
                    perimeter = perimeter - 1
                    edges.remove(n)
            if p > 0:
                on_perimeter[s] = edges
            else:
                interior.add(s)
        sides = perimeter
        for s, edges in on_perimeter.items():
            for n in (s + 1, s + 1j):
                if n in on_perimeter:
                    for e in edges:
                        if e in on_perimeter[n]:
                            sides = sides - 1
        p1_cost = p1_cost + area * perimeter
        p2_cost = p2_cost + area * sides
    return p1_cost, p2_cost


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "12.txt") as f:
        garden_map = lines_to_map(f.read().splitlines())
    p1, p2 = process_map(garden_map.copy())
    print_day(12, p1, p2)
    assert p1 == 1473408
    assert p2 == 886364


if __name__ == '__main__':
    main()
