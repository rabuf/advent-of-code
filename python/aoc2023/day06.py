import sys
from functools import reduce
from math import ceil, floor
from operator import mul
from pathlib import Path

from aoc_util import print_day


# Written after realizing what the formula was, and after submitting solutions.
def fast_ways_to_beat(time, distance):
    lower = (time - (time * time - 4 * distance) ** 0.5) / 2
    upper = (time + (time * time - 4 * distance) ** 0.5) / 2
    return (
        floor(upper)
        - ceil(lower)
        + 1
        - (ceil(lower) == lower)
        - (floor(upper) == upper)
    )


# Used for both parts for the day.
def ways_to_beat(time, distance):
    count = 0
    for speed in range(time):
        count += distance < speed * (time - speed)
    return count


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "06.txt") as f:
        lines = f.readlines()
        times, distances = [line.split(":")[1].split() for line in lines]
    time = int("".join(times))
    distance = int("".join(distances))
    total = reduce(
        mul, (fast_ways_to_beat(int(t), int(d)) for (t, d) in zip(times, distances))
    )
    print_day(6, total, fast_ways_to_beat(time, distance))


if __name__ == "__main__":
    main()
