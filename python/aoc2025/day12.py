import sys
from operator import mul
from pathlib import Path

from aoc_util import print_day

YEAR = "2025"
DAY = "12"


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / YEAR / f"{DAY}.txt") as f:
            lines = f.read().split("\n\n")
        regions = lines[-1].splitlines()
        tile_size = [s.count("#") for s in lines[:-1]]
        part1 = 0
        for r in regions:
            area, counts = r.split(": ")
            area = mul(*map(int, area.split("x")))
            counts = (int(c) for c in counts.split())
            part1 += sum(t * c for t, c in zip(tile_size, counts)) <= area
        print_day(DAY, part1, "freebie!")
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
