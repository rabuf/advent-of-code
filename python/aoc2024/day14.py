import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return line


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "14.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
            print_day(14, len(lines), len(lines))
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()