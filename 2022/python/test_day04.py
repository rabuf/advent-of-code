import pytest

from day04 import *

sample = ("2-4,6-8\n"
          "2-3,4-5\n"
          "5-7,7-9\n"
          "2-8,3-7\n"
          "6-6,4-6\n"
          "2-6,4-8"
          )

sample_parsed = [
    ((2, 4), (6, 8)),
    ((2, 3), (4, 5)),
    ((5, 7), (7, 9)),
    ((2, 8), (3, 7)),
    ((6, 6), (4, 6)),
    ((2, 6), (4, 8)),
]
sample_contains = [False, False, False, True, True, False]
sample_overlaps = [False, False, True, True, True, True]


@pytest.mark.parametrize("line, parsed", zip(sample.split("\n"), sample_parsed))
def test_parse_line(line, parsed):
    assert parsed == parse_line(line)


@pytest.mark.parametrize("ranges, is_contained", zip(sample_parsed, sample_contains))
def test_contains(ranges, is_contained):
    assert contains(ranges) == is_contained


@pytest.mark.parametrize("ranges, is_overlapping", zip(sample_parsed, sample_overlaps))
def test_overlaps(ranges, is_overlapping):
    assert overlaps(ranges) == is_overlapping


if __name__ == '__main__':
    pytest.main()
