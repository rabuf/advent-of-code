import sys
from functools import reduce
from operator import and_
from pathlib import Path

from aoc_util import chunk, print_day


def split(rucksack):
    mid = len(rucksack) // 2
    return rucksack[:mid], rucksack[mid:]


def common_element(*rucksacks):
    sets = [set(rucksack) for rucksack in rucksacks]
    return reduce(and_, sets).pop()


def priority_value(letter):
    if letter.isupper():
        return 27 + ord(letter) - ord("A")
    else:
        return 1 + ord(letter) - ord("a")


def priority_sum(rucksacks):
    return sum(
        priority_value(common_element(*split(rucksack))) for rucksack in rucksacks
    )


def badge_sum(rucksacks):
    chunks = chunk(rucksacks, 3)
    return sum(priority_value(common_element(*c)) for c in chunks)


def main(input_path=Path(sys.argv[1])):
    with open(input_path / "2022" / "03.txt", "r") as f:
        rucksacks = f.read().strip().split("\n")
        print_day(3, priority_sum(rucksacks), badge_sum(rucksacks))


if __name__ == "__main__":
    main()
