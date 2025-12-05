import re
import sys
from dataclasses import dataclass
from functools import reduce
from operator import add, floordiv, mod, mul
from pathlib import Path
from typing import Callable

from aoc_util import chunk_fill, print_day


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
    r = re.compile(r"Operation: new = (old|\d+) (\*|\+) (old|\d+)")
    m = r.search(expr)
    match m.group(2):
        case "*":
            op = mul
        case "+":
            op = add
    match m.group(1):
        case "old":
            lhs = "old"
        case n:
            lhs = int(n)
    match m.group(3):
        case "old":
            rhs = "old"
        case n:
            rhs = int(n)

    match (lhs, rhs):
        case ("old", "old"):
            operation = lambda old: op(old, old)  # noqa: E731
        case ("old", n):
            operation = lambda old: op(old, n)  # noqa: E731
        case (n, "old"):
            operation = lambda old: op(n, old)  # noqa: E731
        case (m, n):
            operation = lambda old: op(m, n)  # noqa: E731

    return operation


def parse_monkey(lines):
    r = re.compile(r"\d+")
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


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2022" / "11.txt") as f:
        lines = f.read().splitlines()
        monkeys = list(map(parse_monkey, chunk_fill(lines, 7)))
        part1 = monkey_business(monkeys)
        monkeys = list(map(parse_monkey, chunk_fill(lines, 7)))
        divisor = reduce(mul, [m.divisor for m in monkeys])
        part2 = monkey_business(monkeys, rounds=10000, op=mod, divisor=divisor)
        print_day(11, part1, part2)


if __name__ == "__main__":
    main()
