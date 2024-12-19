import sys
from pathlib import Path

from aoc2019 import intcode
from aoc_util import print_day


def parse_line(line):
    return line


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "09.txt") as f:
            program = intcode.parse_program(f.read())
        output = []
        intcode.v3(program, read=lambda: 1, write=lambda n: output.append(n))
        intcode.v3(program, read=lambda: 2, write=lambda n: output.append(n))
        print_day("09", output[0], output[1])
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
