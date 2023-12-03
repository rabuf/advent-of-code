from aoc2023 import day03

sample = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""


part1_parts = [467, 35, 633, 617, 592, 755, 664, 598]


def test_find_parts():
    assert day03.find_parts(sample.splitlines()) == part1_parts


def test_part1():
    assert sum(day03.find_parts(sample.splitlines())) == 4361


def test_gear_ratios():
    assert day03.find_gear_ratios(sample.splitlines()) == [16345, 451490]

def test_parse_line():
    assert True
