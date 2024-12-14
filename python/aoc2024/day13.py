import sys
from functools import partial
from pathlib import Path

import regex as re
import z3

from aoc_util import print_day


def parse_machine(machine):
    nums = tuple(map(int, re.findall(r'\d+', machine)))
    return nums


def machine_cost(machine, offset=0):
    s = z3.Optimize()
    ax, ay, bx, by, px, py = machine
    px, py = px + offset, py + offset
    a, b = z3.Ints('a b')
    cost = a * 3 + b
    s.add(a * ax + b * bx == px)
    s.add(a * ay + b * by == py)
    s.minimize(cost)
    if s.check() == z3.sat:
        m = s.model()
        return m.eval(cost).as_long()
    else:
        return 0


def machine_cost_math(machine, offset=0):
    ax, ay, bx, by, px, py = machine
    px, py = px + offset, py + offset
    a = (px * by - py * bx) // (ax * by - ay * bx)
    b = (py - a * ay) // by
    if (a * ax + b * bx, a * ay + b * by) == (px, py):
        return a * 3 + b
    return 0


def faster_z3(machines, offset=0):
    result = 0
    s = z3.Solver()
    a, b = z3.Ints('a b')
    cost = a * 3 + b
    for ax, ay, bx, by, px, py in machines:
        px += offset
        py += offset
        s.push()
        s.add(a * ax + b * bx == px)
        s.add(a * ay + b * by == py)
        if s.check() == z3.sat:
            m = s.model()
            result += m.eval(cost).as_long()
        s.pop()
    return result


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "13.txt") as f:
            machines = list(map(parse_machine, f.read().split('\n\n')))
        p1 = sum(map(machine_cost_math, machines))
        p2 = sum(map(partial(machine_cost_math, offset=10000000000000), machines))
        print_day(13, p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
