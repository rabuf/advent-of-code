from aoc2023.day05 import *


def test_next_range_below():
    assert next_ranges({range(1, 10), range(1, 10)}, {range(20, 30): 10}) == [range(1, 10)]


def test_next_range_above():
    assert next_ranges([range(1, 10)], {range(-10, 0): 30}) == [range(1, 10)]


def test_next_range_inside():
    assert next_ranges([range(1, 4)], {range(0, 5): 3}) == [range(4, 7)]


def test_next_range_surrounds():
    assert next_ranges([range(0, 10)], {range(2, 5): 3}) == [range(3, 6), range(0, 2), range(5, 10)]
