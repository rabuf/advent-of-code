import re
import sys
from itertools import product
from pathlib import Path

from aoc2019 import intcode
from aoc_util import print_day


def parse_line(line):
    return list(map(int, re.findall(r'\d+', line)))


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "02.txt") as f:
            program = parse_line(f.read())
            p1 = intcode.v1(program, {1: 12, 2: 2})
            p2 = 0
            for noun, verb in product(range(100), repeat=2):
                if intcode.v1(program, {1: noun, 2: verb}) == 19690720:
                    p2 = 100 * noun + verb
                    break
            print_day("02", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
