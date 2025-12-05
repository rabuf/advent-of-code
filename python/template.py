import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return line


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "YEAR" / "DAY.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        print_day("DAY", lines, len(lines))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
