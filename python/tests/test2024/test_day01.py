from aoc2024.day01 import *

from pytest import fixture

sample = """3   4
4   3
2   5
1   3
3   9
3   3"""


@fixture
def sample_input():
    return [parse_line(line) for line in sample.splitlines()]


def test_part1(sample_input):
    assert part1(*zip(*sample_input)) == 11


def test_part2(sample_input):
    assert part2(*zip(*sample_input)) == 31