from pytest import fixture

from aoc2024.day12 import *

sample = """RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"""

xos = """OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"""

abcde = """AAAA
BBCD
BBCC
EEEC"""


@fixture
def sample_grid():
    return lines_to_map(sample.splitlines())


@fixture
def xos_grid():
    return lines_to_map(xos.splitlines())


@fixture
def abcde_grid():
    return lines_to_map(abcde.splitlines())


def test_part1(sample_grid):
    p1, p2 = process_map(sample_grid)
    assert p1 == 1930
    assert p2 == 1206


def test_part1_xo(xos_grid):
    p1, p2 = process_map(xos_grid)
    assert p1 == 772
    assert p2 == 436


def test_part1_abcde(abcde_grid):
    p1, p2 = process_map(abcde_grid)
    assert p1 == 140
    assert p2 == 80
