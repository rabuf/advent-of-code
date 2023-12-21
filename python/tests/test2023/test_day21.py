from pytest import fixture, mark, skip

from aoc2023.day21 import *


sample = """...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
"""

@fixture
def sample_input():
    lines = sample.splitlines()
    return lines_to_grid(lines)


@mark.parametrize('steps, gardens', [(0, 1), (1, 2), (6, 16)])
def test_sample_input(sample_input, steps, gardens):
    grid, start = sample_input
    assert gardens_within(grid, start, steps) == gardens
