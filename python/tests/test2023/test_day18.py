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
