import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return line


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "12.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        print_day(12, len(lines), len(lines))


if __name__ == '__main__':
    main()
