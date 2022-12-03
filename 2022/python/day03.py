import sys
from functools import reduce
from pathlib import Path

from aoc_util import *


def split(rucksack):
    mid = len(rucksack) // 2
    return rucksack[:mid], rucksack[mid:]


def common_element(*rucksacks):
    sets = [set(rucksack) for rucksack in rucksacks]
    result = reduce(lambda a, b: a.intersection(b), sets)
    return result.pop()


def priority_value(letter):
    if letter.isupper():
        return 27 + ord(letter) - ord('A')
    else:
        return 1 + ord(letter) - ord('a')


def priority_sum(rucksacks):
    return sum(priority_value(common_element(*split(rucksack))) for rucksack in rucksacks)


def badge_sum(rucksacks):
    chunks = chunk(rucksacks, 3)
    return sum(priority_value(common_element(*c)) for c in chunks)


def main():
    input_path = Path(sys.argv[1])
    with open(input_path / "03.txt", "r") as f:
        print("Day 02:")
        rucksacks = f.read().strip().split("\n")
        a, b = priority_sum(rucksacks), badge_sum(rucksacks)
        print(f"\tPart 1: {a}")
        print(f"\tPart 2: {b}")


if __name__ == "__main__":
    main()
