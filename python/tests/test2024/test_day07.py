import pytest

from aoc2024.day07 import *

sample = """190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"""

part1 = list(zip((parse_line(line) for line in sample.splitlines()),
                 [True, True, False, False, False, False, False, False, True]))
part2 = list(
    zip((parse_line(line) for line in sample.splitlines()), [True, True, False, True, True, False, True, False, True]))


@pytest.mark.parametrize('test_case, valid', part1)
def test_add_mul(test_case, valid):
    expected, operands = test_case
    assert valid == find_operands(expected, operands)


@pytest.mark.parametrize('test_case, valid', part2)
def test_add_mul_catenate(test_case, valid):
    expected, operands = test_case
    assert valid == find_operands(expected, operands, operations=(mul, add, catenate))
