import pytest

import aoc_util
from aoc2022 import day08

sample = ("30373\n"
          "25512\n"
          "65332\n"
          "33549\n"
          "35390")

sample_grid = [[3, 0, 3, 7, 3],
               [2, 5, 5, 1, 2],
               [6, 5, 3, 3, 2],
               [3, 3, 5, 4, 9],
               [3, 5, 3, 9, 0]]


def test_parse_trees():
    assert aoc_util.input_to_grid(sample.splitlines(), int) == sample_grid


def test_count_visible_trees():
    assert day08.count_visible_trees(sample_grid) == 21


@pytest.mark.parametrize('x,y,score', [(1, 2, 4), (3, 2, 8)])
def test_scenic_score(x, y, score):
    assert day08.scenic_score(sample_grid, x, y) == score
