import re
import sys
from pathlib import Path

from more_itertools import chunked

from aoc_util import print_day


def parse_line(line):
    pattern = r"\d+-\d+"
    return [
        (int(lower), int(upper))
        for lower, upper in map(lambda s: s.split("-"), re.findall(pattern, line))
    ]


def invalid(lower, upper):
    # magic = 11
    result = 0
    for i in range(lower, upper + 1):
        s = str(i)
        if len(s) % 2 == 0:
            if s[0 : len(s) // 2] == s[len(s) // 2 :]:
                result += i
    return result


def part1(ranges):
    return sum(invalid(lower, upper) for lower, upper in ranges)


def invalid2(lower, upper):
    result = 0
    for i in range(lower, upper + 1):
        s = str(i)
        for j in range(2, len(s) + 1):
            if len(s) % j == 0:
                prefix = s[0 : len(s) // j]
                chunks = chunked(s, len(s) // j)
                if all(prefix == "".join(chunk) for chunk in chunks):
                    result += i
                    break
    return result


def part2(ranges):
    return sum(invalid2(lower, upper) for lower, upper in ranges)


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2025" / "02.txt") as f:
            line = parse_line(f.read())
        print_day("02", part1(line), part2(line))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
