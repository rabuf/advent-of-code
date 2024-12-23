from aoc2023.day11 import *


def test_manhattan():
    assert manhattan((1, 6), (5, 11)) == 9
    assert manhattan((4, 0), (9, 10)) == 15
