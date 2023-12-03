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


sample2 = """
12.......*..
+.........34
.......-12..
..78........
..*....60...
78.........9
.5.....23..$
8...90*12...
............
2.2......12.
.*.........*
1.1..503+.56
"""


def test_sample2():
    assert sum(day03.find_parts(sample2.splitlines())) == 925
    assert sum(day03.find_gear_ratios(sample2.splitlines())) == 6756


def test_find_parts():
    assert day03.find_parts(sample.splitlines()) == part1_parts


def test_part1():
    assert sum(day03.find_parts(sample.splitlines())) == 4361


def test_gear_ratios():
    assert day03.find_gear_ratios(sample.splitlines()) == [16345, 451490]
