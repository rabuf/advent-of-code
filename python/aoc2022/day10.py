import sys
from pathlib import Path

from aoc_util import print_day


def parse(line):
    match line.split():
        case ['noop']:
            pass
        case ['addx', num]:
            return int(num)


def parse_lines(lines):
    return [parse(line) for line in lines]


def cycle(op, state: tuple[int, int]) -> tuple[int, int]:
    x, v = state
    match op:
        case None:
            yield x + v, 0
        case int as n:
            yield x + v, 0
            yield x + v, n


def apply(instructions):
    state = (1, 0)
    for instruction in instructions:
        for next_state in cycle(instruction, state):
            state = next_state
            yield next_state[0]


def sum_signal_strengths(instructions):
    total = 0
    for i, val in enumerate(apply(instructions), 1):
        if (i - 20) % 40 == 0:
            total = total + i * val
    return total


def render(instructions):
    s = ''
    for index, pos in enumerate(apply(instructions), 0):
        t = index % 40
        s += '#' if t - 1 <= pos <= t + 1 else ' '
        if index % 40 == 39:
            s += '\n'
    return s


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2022" / "10.txt") as f:
        instructions = parse_lines(f.read().splitlines())
        print_day(10, sum_signal_strengths(instructions), '\n' + render(instructions))


if __name__ == '__main__':
    main()
