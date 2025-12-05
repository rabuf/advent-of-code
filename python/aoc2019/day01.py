import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return int(line)


def fuel(n):
    return n // 3 - 2


def rocket(n):
    result = fuel(n)
    f = result
    while (f := fuel(f)) > 0:
        result = result + f
    return result


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2019" / "01.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
            p1 = sum(map(fuel, lines))
            p2 = sum(map(rocket, lines))
            print_day("01", p1, p2)
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
