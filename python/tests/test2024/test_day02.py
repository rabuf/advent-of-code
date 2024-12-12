from pytest import fixture

from aoc2024.day02 import *

sample = """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"""


@fixture
def sample_input():
    return [parse_line(line) for line in sample.splitlines()]


def test_part1(sample_input):
    assert 2 == sum(safe(record) for record in sample_input)


def test_part2(sample_input):
    assert 4 == sum(make_safe(record) for record in sample_input)


def test_unsafe():
    assert not safe([9, 7, 6, 2, 1])
