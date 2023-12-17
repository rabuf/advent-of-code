from aoc2023.day17 import *

sample = """2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
"""

sample_2 = """111111111111
999999999991
999999999991
999999999991
999999999991
"""


def test_part1():
    lines = sample.splitlines()
    height, width = len(lines), len(lines[0])
    grid = {(x, y): int(n) for y, line in enumerate(lines) for x, n in enumerate(line)}
    assert least_heat(grid, height, width, max_path=3) == 102


def test_part2():
    lines = sample.splitlines()
    height, width = len(lines), len(lines[0])
    grid = {(x, y): int(n) for y, line in enumerate(lines) for x, n in enumerate(line)}
    assert least_heat(grid, height, width, min_path=3, max_path=10) == 94


def test_part2_sample2():
    lines = sample_2.splitlines()
    height, width = len(lines), len(lines[0])
    grid = {(x, y): int(n) for y, line in enumerate(lines) for x, n in enumerate(line)}
    assert least_heat(grid, height, width, min_path=3, max_path=10) == 71
