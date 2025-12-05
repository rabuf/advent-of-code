import sys
from functools import cmp_to_key
from pathlib import Path

from aoc_util import print_day


def part1(fresh, ingredients):
    return sum(any(ingredient in f for f in fresh) for ingredient in ingredients)


def cmp_ranges(a, b):
    if a[0] < b[0]:
        return -1
    if a[0] > b[0]:
        return 1
    if a[-1] < b[-1]:
        return -1
    if a[-1] > b[-1]:
        return -1
    return 0


def part2(fresh):
    fresh = list(sorted(fresh, key=cmp_to_key(cmp_ranges)))
    merged = [range(fresh[0][0], fresh[0][-1] + 1)]
    for start, end in [(r[0], r[-1]) for r in fresh]:
        merge_start, merge_end = merged[-1][0], merged[-1][-1]
        if start <= merge_end:
            merged[-1] = range(merge_start, max(merge_end, end) + 1)
        else:
            merged.append(range(start, end + 1))
    return sum(len(m) for m in merged)


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2025" / "05.txt") as f:
            fresh, ingredients = f.read().split("\n\n")
        fresh = [
            range(int(lower), int(upper) + 1)
            for lower, upper in (f.split("-") for f in fresh.splitlines())
        ]
        ingredients = map(int, ingredients.splitlines())
        print_day("DAY", part1(fresh, ingredients), part2(fresh))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
