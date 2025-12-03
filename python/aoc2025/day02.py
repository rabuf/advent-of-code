import re
import sys
from pathlib import Path

from more_itertools import chunked

from aoc_util import print_day


def parse_line(line):
    pattern = r"\d+-\d+"
    return [
        range(int(lower), int(upper) + 1)
        for lower, upper in map(lambda s: s.split("-"), re.findall(pattern, line))
    ]


def invalid(r: range):
    result = 0
    for i in r:
        s = str(i)
        if len(s) % 2 == 0:
            if s[0 : len(s) // 2] == s[len(s) // 2 :]:
                result += i
    return result


def part1(ranges):
    return sum(map(invalid, ranges))


def invalid2(r: range):
    result = 0
    for i in r:
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
    return sum(map(invalid2, ranges))


def combined(ranges: list[range]):
    p1, p2 = 0, 0
    r1, r2 = re.compile(r"^(\d+)\1$"), re.compile(r"^(\d+)\1+$")
    for r in ranges:
        for i in r:
            s = str(i)
            if r1.match(s):
                p1 += i
            if r2.match(s):
                p2 += i
    return p1, p2


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2025" / "02.txt") as f:
            line = parse_line(f.read())
        print_day("02", *combined(line))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
