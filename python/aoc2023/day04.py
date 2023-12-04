import sys
from pathlib import Path

from aoc_util import print_day


def score(winning, numbers):
    matches = len(winning & numbers)
    return 2 ** (matches - 1) if matches else 0


def copies(cards):
    counts = [1] * len(cards)
    for (i, (winning, numbers)) in enumerate(cards, start=0):
        matches = len(winning & numbers)
        for j in range(i + 1, i + 1 + matches):
            counts[j] += counts[i]
    return counts


def parse_line(line: str):
    winning, numbers = line.split(':')[1].split('|')
    return set(winning.split()), set(numbers.split())


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "04.txt") as f:
        cards = [parse_line(line) for line in f.read().splitlines()]
        p1 = sum(score(*card) for card in cards)
        p2 = sum(copies(cards))
        print_day(4, p1, p2)


if __name__ == '__main__':
    main()
