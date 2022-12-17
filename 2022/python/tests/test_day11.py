from functools import reduce
from operator import mod, mul

import pytest
from hypothesis import given, strategies as st

from aoc2022 import day11
from aoc_util import chunk_fill

sample = ("Monkey 0:\n"
          "  Starting items: 79, 98\n"
          "  Operation: new = old * 19\n"
          "  Test: divisible by 23\n"
          "    If true: throw to monkey 2\n"
          "    If false: throw to monkey 3\n"
          "\n"
          "Monkey 1:\n"
          "  Starting items: 54, 65, 75, 74\n"
          "  Operation: new = old + 6\n"
          "  Test: divisible by 19\n"
          "    If true: throw to monkey 2\n"
          "    If false: throw to monkey 0\n"
          "\n"
          "Monkey 2:\n"
          "  Starting items: 79, 60, 97\n"
          "  Operation: new = old * old\n"
          "  Test: divisible by 13\n"
          "    If true: throw to monkey 1\n"
          "    If false: throw to monkey 3\n"
          "\n"
          "Monkey 3:\n"
          "  Starting items: 74\n"
          "  Operation: new = old + 3\n"
          "  Test: divisible by 17\n"
          "    If true: throw to monkey 0\n"
          "    If false: throw to monkey 1")

sample_expected = [
    (0, [79, 98], lambda old: old * 19, 23, 2, 3),
    (1, [54, 65, 75, 74], lambda old: old + 6, 19, 2, 0),
    (2, [79, 60, 97], lambda old: old * old, 13, 1, 3),
    (3, [74], lambda old: old + 3, 17, 0, 1),
]


def test_splitting_monkeys():
    monkeys = list(chunk_fill(sample.splitlines(), 7))
    assert len(monkeys) == 4


@pytest.mark.parametrize('monkey, expected', zip(chunk_fill(sample.splitlines(), 7), sample_expected))
def test_parse_monkey(monkey, expected):
    monkey = day11.parse_monkey(monkey)
    assert monkey.number == expected[0]
    assert monkey.items == expected[1]
    assert callable(monkey.new)
    assert monkey.divisor == expected[3]
    assert monkey.true == expected[4]
    assert monkey.false == expected[5]


@pytest.mark.parametrize('monkey, expected', zip(chunk_fill(sample.splitlines(), 7), sample_expected))
@given(n=st.integers())
def test_verify_operation_parsing(monkey, expected, n):
    monkey_operation = day11.parse_monkey(monkey).new
    expected_operation = expected[2]
    assert callable(expected_operation)
    assert monkey_operation(old=n) == expected_operation(n)


def test_single_round():
    monkeys = [day11.parse_monkey(m) for m in chunk_fill(sample.splitlines(), 7)]
    day11.round(monkeys)
    assert monkeys[0].items == [20, 23, 27, 26]
    assert monkeys[1].items == [2080, 25, 167, 207, 401, 1046]


def test_monkey_business():
    monkeys = [day11.parse_monkey(m) for m in chunk_fill(sample.splitlines(), 7)]
    assert day11.monkey_business(monkeys, rounds=20) == 10605


def test_worrywart():
    monkeys = [day11.parse_monkey(m) for m in chunk_fill(sample.splitlines(), 7)]
    assert day11.monkey_business(monkeys, rounds=10000, op=mod,
                                 divisor=reduce(mul, [m.divisor for m in monkeys])) == 2713310158
