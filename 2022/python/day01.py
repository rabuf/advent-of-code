import sys
from pathlib import Path

from aoc_util import print_day


def solve(elves):
    return elves[-1], sum(elves[-3:])


def parse(elves):
    result = []
    for elf in elves.split("\n\n"):
        calories = sum(int(calories) for calories in elf.split("\n"))
        result.append(calories)
    return sorted(result)


def main():
    input_path = Path(sys.argv[1])
    with open(input_path / "01.txt", "r") as f:
        elves = parse(f.read().strip())
        print_day(1, *solve(elves))


if __name__ == "__main__":
    main()
