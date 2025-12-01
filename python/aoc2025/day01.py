import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line: str):
    lr, n = line[0], int(line[1:])
    return lr, n


def part1(lines):
    position = 50
    password = 0
    for dir, count in lines:
        if position == 0:
            password = password + 1
        if dir == "L":
            position = (position - count) % 100
        else:
            position = (position + count) % 100
    return password


def part2(lines):
    position = 50
    password = 0
    for direction, count in lines:
        password = password + count // 100
        turns = count % 100
        if direction == "L" and position > 0 >= position - turns:
            password = password + 1
        elif direction == "R" and position + turns >= 100:
            password = password + 1
        position = position + (-turns if direction == "L" else turns)
        position = position % 100

    return password


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2025" / "01.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        p1 = part1(lines)
        p2 = part2(lines)
        print_day(1, p1, p2)


if __name__ == "__main__":
    main()
