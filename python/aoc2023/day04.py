import sys
from pathlib import Path
import re

from aoc_util import print_day


def score(winning, numbers):
    matches = set(winning).intersection(numbers)
    return 2**(len(matches) - 1) if matches else 0


def copies(cards):
    counts = [1] * len(cards)
    for i in range(len(cards)):
        winning, numbers = cards[i]
        matches = len(set(winning).intersection(numbers))
        for j in range(i+1, i+1+matches):
            counts[j] += counts[i]
    return counts


def parse_line(line):
    winning, numbers = line.split(':')[1].split(' | ')
    winning = re.findall(r'\d+', winning)
    numbers = re.findall(r'\d+', numbers)
    return winning, numbers


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "04.txt") as f:
        cards = [parse_line(line) for line in f.read().splitlines()]
        p1 = sum(score(*card) for card in cards)
        p2 = sum(copies(cards))
        print_day(4, p1, p2)


if __name__ == '__main__':
    main()
