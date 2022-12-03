import sys
from pathlib import Path


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
        print("Day 01:")
        a, b = solve(elves)
        print(f"\tPart 1: {a}")
        print(f"\tPart 2: {b}")


if __name__ == "__main__":
    main()
