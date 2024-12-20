import sys
from collections import defaultdict
from pathlib import Path

from aoc2019 import intcode
from aoc_util import print_day


def parse_line(line):
    return line


def play(program):
    program = program.copy()
    program[0] = 2
    state = 0
    x = y = 0
    ball = paddle = 0
    score = 0

    def joystick():
        return (paddle < ball) - (paddle > ball)

    def game(n):
        nonlocal x, y, ball, paddle, score, state
        match state:
            case 0:
                x = n
            case 1:
                y = n
            case 2:
                if x == -1 and y == 0:
                    score = n
                elif n == 4:
                    ball = x
                elif n == 3:
                    paddle = x
        state = (state + 1) % 3

    intcode.v3(program, read=joystick, write=game)

    return score


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "13.txt") as f:
            program = intcode.parse_program(f.read())
        write, read, t = intcode.run_with_queues(program)
        t.join()
        screen = defaultdict(lambda: ' ')
        while read.qsize() > 0:
            x = read.get()
            y = read.get()
            kind = read.get()
            screen[(x, y)] = kind
        p1 = sum(v == 2 for _, v in screen.items())
        p2 = play(program)
        print_day("13", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
