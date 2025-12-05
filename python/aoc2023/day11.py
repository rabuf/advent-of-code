import sys
from pathlib import Path

from more_itertools import transpose

from aoc_util import print_day


def empty_space(space: list[str]) -> tuple[list[int], list[int]]:
    rows = []
    columns = []
    for i, line in enumerate(space):
        if not line.count("#"):
            rows.append(i)
    for j, column in enumerate(transpose(space)):
        if not column.count("#"):
            columns.append(j)
    return rows, columns


def galaxy_locations(space: list[str]) -> list[tuple[int, int]]:
    locations = []
    for j, row in enumerate(space):
        for i, col in enumerate(row):
            if col == "#":
                locations.append((i, j))
    return locations


def manhattan(g1: tuple[int, int], g2: tuple[int, int]) -> int:
    return abs(g1[0] - g2[0]) + abs(g1[1] - g2[1])


def all_pair_distances(locations, empty_rows, empty_columns, gap=1):
    distances = []
    for i, a in enumerate(locations):
        for b in locations[i + 1 :]:
            ax, ay = a
            bx, by = b
            distance = 0
            for row in empty_rows:
                if row in range(ay, by, -1 if by < ay else 1):
                    distance += gap
            for col in empty_columns:
                if col in range(ax, bx, -1 if bx < ax else 1):
                    distance += gap
            distances.append(manhattan(a, b) + distance)
    return distances


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "11.txt") as f:
        space = f.read().splitlines()
        locations = galaxy_locations(space)
        empty_rows, empty_columns = empty_space(space)
        distances = all_pair_distances(locations, empty_rows, empty_columns, gap=1)
        total = sum(distances)
        distances = all_pair_distances(locations, empty_rows, empty_columns, gap=999999)
        p2_total = sum(distances)
        print_day(11, total, p2_total)


if __name__ == "__main__":
    main()
