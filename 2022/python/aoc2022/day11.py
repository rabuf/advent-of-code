import re
import sys
from dataclasses import dataclass
from functools import reduce
from operator import add, mul, mod, floordiv
from pathlib import Path
from typing import Callable

from aoc_util import print_day, chunk_fill


@dataclass
class Monkey:
    number: int
    items: list[int]
    new: Callable[[int], int]
    divisor: int
    true: int
    false: int
    examined: int = 0


def parse_expression(expr):
    r = re.compile(r'Operation: new = (old|\d+) (\*|\+) (old|\d+)')
    m = r.search(expr)
    match m.group(2):
        case '*':
            op = mul
        case '+':
            op = add
    match m.group(1):
        case 'old':
            lhs = 'old'
        case n:
            lhs = int(n)
    match m.group(3):
        case 'old':
            rhs = 'old'
        case n:
            rhs = int(n)

    def operation(**kwargs):
        l = kwargs['old'] if lhs == 'old' else lhs
        r = kwargs['old'] if rhs == 'old' else rhs
        return op(l, r)

    return operation


def parse_monkey(lines):
    r = re.compile(r'\d+')
    n = int(r.search(lines[0])[0])
    items = list(map(int, r.findall(lines[1])))
    op = parse_expression(lines[2])
    divisor = int(r.search(lines[3])[0])
    true = int(r.search(lines[4])[0])
    false = int(r.search(lines[5])[0])
    return Monkey(n, items, op, divisor, true, false)


def round(monkeys, op=floordiv, divisor=3):
    for m in monkeys:
        for w in m.items:
            worry = op(m.new(old=w), divisor)
            monkeys[m.true if worry % m.divisor == 0 else m.false].items.append(worry)
            m.examined += 1
        m.items = []


def monkey_business(monkeys, rounds=20, op=floordiv, divisor=3):
    for _ in range(rounds):
        round(monkeys, op=op, divisor=divisor)
    return mul(*sorted([m.examined for m in monkeys])[-2:])


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "11.txt") as f:
        lines = f.read().splitlines()
        monkeys = list(map(parse_monkey, chunk_fill(lines, 7)))
        part1 = monkey_business(monkeys)
        monkeys = list(map(parse_monkey, chunk_fill(lines, 7)))
        part2 = monkey_business(monkeys, rounds=10000, op=mod, divisor=reduce(mul, [m.divisor for m in monkeys]))
        print_day(11, part1, part2)


if __name__ == '__main__':
    main()
