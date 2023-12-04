from pytest import mark

from aoc2023.day04 import *

sample = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""

expected_scores = [8, 2, 2, 1, 0, 0]

expected_counts = [1, 2, 4, 8, 14, 1]


@mark.parametrize("card,expected", zip(sample.splitlines(), expected_scores))
def test_scoring(card, expected):
    winning, numbers = parse_line(card)
    assert score(winning, numbers) == expected


def test_copies():
    cards = [parse_line(line) for line in sample.splitlines()]
    count = copies(cards)
    assert sum(count) == 30


def test_copies_counts():
    cards = [parse_line(line) for line in sample.splitlines()]
    count = copies(cards)
    assert count == expected_counts
