from aoc2023.day12 import *

from pytest import mark

sample_data = """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""

sample_results = [1, 4, 1, 1, 4, 10]

sample_regexes = [r'\.*#\.+#\.+###\.*',
                  r'\.*#\.+#\.+###\.*',
                  r'\.*#\.+###\.+#\.+######\.*',
                  r'\.*####\.+#\.+#\.*',
                  r'\.*#\.+######\.+#####\.*',
                  r'\.*###\.+##\.+#\.*',
                  ]


@mark.parametrize("record, configurations", zip((parse_line(line) for line in sample_data.splitlines()), sample_results))
def test_part1(record, configurations):
    assert configuration_count(record) == configurations


@mark.parametrize("record, regex", zip((parse_line(line) for line in sample_data.splitlines()), sample_regexes))
def test_regex_generation(record, regex):
    assert groups_to_regex(record.groups) == re.compile(regex)

