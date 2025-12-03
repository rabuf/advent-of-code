import sys
from functools import partial
from pathlib import Path

from aoc_util import print_day


def best_joltage(bank, size=2):
    joltage, rest = bank[:size], bank[size:]
    for c in rest:
        best = joltage
        for i in range(size):
            contender = joltage[:i] + joltage[i + 1 :] + c
            best = max(best, contender)
        joltage = best
    return int(joltage)


def part1(banks):
    return sum(map(best_joltage, banks))


def part2(banks):
    return sum(map(partial(best_joltage, size=12), banks))


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2025" / "03.txt") as f:
            banks = f.read().splitlines()
        print_day("03", part1(banks), part2(banks))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
