import pytest

from aoc_util import chunk
from day03 import *

sample = ('vJrwpWtwJgWrhcsFMMfFFhFp\n'
          'jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n'
          'PmmdzqPrVvPwwTWBwg\n'
          'wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n'
          'ttgJtRGJQctTZtZT\n'
          'CrZsJsPPZsGzwwsLwLmpwMDw'
          )

sample_priority_letters = ['p', 'L', 'P', 'v', 't', 's']
sample_priority_values = [16, 38, 42, 22, 20, 19]
sample_priority_sum = 157
sample_badges = ['r', 'Z']
sample_badge_priorities = [18, 52]
sample_badge_priority_sum = 70


@pytest.mark.parametrize("rucksack", sample.split())
def test_split_rucksack_equal_size(rucksack):
    left, right = split(rucksack)
    assert len(left) == len(right)


@pytest.mark.parametrize("rucksack", sample.split())
def test_split_rucksack_preserves_values(rucksack):
    left, right = split(rucksack)
    assert rucksack == left + right


@pytest.mark.parametrize("rucksack,priority_letter", zip(sample.split(), sample_priority_letters))
def test_common_element(rucksack, priority_letter):
    assert common_element(*split(rucksack)) == priority_letter


@pytest.mark.parametrize("priority_letter,pv", zip(sample_priority_letters, sample_priority_values))
def test_priority_values(priority_letter, pv):
    assert priority_value(priority_letter) == pv


def test_priority_sum():
    assert priority_sum(sample.split()) == sample_priority_sum


@pytest.mark.parametrize("group, badge", zip(chunk(sample.split(), 3), sample_badges))
def test_group_badge(group, badge):
    assert common_element(*group) == badge


def test_group_badge_priority_sum():
    assert badge_sum(sample.split()) == sample_badge_priority_sum


if __name__ == '__main__':
    pytest.main()
