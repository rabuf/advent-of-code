from pytest import mark

from aoc2023.day01 import *

day1_sample = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
twone
eighthree
oneight
"""

day1_part2_interp = [29, 83, 13, 24, 42, 14, 76, 21, 83, 18]


@mark.parametrize("line,expected", zip(day1_sample.splitlines(), day1_part2_interp))
def test_day1_part2_parsing(line, expected):
    assert line_to_calibration_value(line, r'(\d|one|two|three|four|five|six|seven|eight|nine)') == expected


def test_day1_part2_sample():
    values = (line_to_calibration_value(line, r'(\d|one|two|three|four|five|six|seven|eight|nine)')
              for line in day1_sample.splitlines())
    assert sum(values) == sum(day1_part2_interp)
