from pytest import fixture, mark

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


@mark.parametrize('steps, gardens', [(6, 16), (10, 50), (50, 1594), (100, 6536), (500, 167004), (1000, 668697)])
def test_infinite_gardens(steps, gardens):
    assert infinite_gardens(sample.splitlines(), (5, 5), steps)[-1] == gardens
