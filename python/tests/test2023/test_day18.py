from aoc2023.day18 import *

sample = """R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"""


def dig_out(lines):
    grid = {}
    position = 0
    min_x, min_y, max_x, max_y = 0, 0, 0, 0
    for direction, length, color in lines:
        for _ in range(length):
            position += direction
            grid[position] = color
            min_x = min(min_x, position.real)
            max_x = max(max_x, position.real)
            min_y = min(min_y, position.imag)
            max_y = max(max_y, position.imag)
    return grid, min_x + min_y * 1j, max_x + max_y * 1j


def fill_in(grid, top_left, bottom_right):
    frontier = {1 + 1j}
    visited = set()
    while frontier:
        consider = frontier.pop()
        if consider.imag < top_left.imag or consider.imag > bottom_right.imag:
            continue
        if consider.real < top_left.real or consider.real > bottom_right.real:
            continue
        if consider not in visited:
            new = {consider + direction for direction in [1, -1, 1j, -1j]}
            new = new - set(grid)
            frontier = frontier | new
            grid[consider] = '#'


def test_day18():
    lines = list(map(parse_line, sample.splitlines()))
    path = [(direction, length) for direction, length, _ in lines]
    grid, top_left, bottom_right = dig_out(lines)
    fill_in(grid, top_left, bottom_right)
    assert len(grid) == 62
    coords = coordinates(path)
    assert shoelace(coords) == 62
    decoded = [decode(color) for _, _, color in lines]
    decoded_coords = coordinates(decoded)
    assert shoelace(decoded_coords) == 952408144115
