from pytest import mark

from aoc2023.day12 import *

sample_data = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""

sample_results = [1, 4, 1, 1, 4, 10]
sample_unfolded_results = [1, 16384, 1, 16, 2500, 506250]


@mark.parametrize("record, configurations",
                  zip((parse_line(line) for line in sample_data.splitlines()), sample_results))
def test_part1_fast(record, configurations):
    assert configuration_count(*record) == configurations


@mark.parametrize("record, configurations",
                  zip((parse_line(line) for line in sample_data.splitlines()), sample_unfolded_results))
def test_part2_fast(record, configurations):
    unfolded = unfold(record)
    assert configuration_count(*unfolded) == configurations


def test_unfold():
    assert unfold(('...', [1, 2])) == ('...?...?...?...?...', [1, 2, 1, 2, 1, 2, 1, 2, 1, 2])


def test_unfold_increases_by_five():
    row, groups = '.?.', [1]
    row_unfolded, groups_unfolded = unfold((row, groups))
    assert row_unfolded.count(row) == 5
    assert len(groups_unfolded) == 5 * len(groups)
