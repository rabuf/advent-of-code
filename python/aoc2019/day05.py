import sys
from pathlib import Path

from aoc2019 import intcode
from aoc_util import print_day


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "05.txt") as f:
            program = intcode.parse_program(f.read())
            output = []
            intcode.v2(program, read=lambda: 1, write=lambda n: output.append(n))
            p1 = output[-1]
            output = []
            intcode.v2(program, read=lambda: 5, write=lambda n: output.append(n))
            p2 = output[-1]
            print_day("02", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
