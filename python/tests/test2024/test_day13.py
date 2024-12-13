import pytest

from aoc2024.day13 import *

machine_text = """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"""

machines = list(map(parse_machine, machine_text.split('\n\n')))


@pytest.mark.parametrize('m, cost', zip(machines, [280, 0, 200, 0]))
def test_machines_part1(m, cost):
    assert machine_cost(m) == cost


@pytest.mark.parametrize('m, cost', zip(machines, [0, 459236326669, 0, 416082282239]))
def test_machines_part2(m, cost):
    assert machine_cost(m, offset=10000000000000) == cost
