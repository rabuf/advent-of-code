import itertools
import sys
from collections import defaultdict
from pathlib import Path

from aoc_util import print_day


def lines_to_grid(lines):
    antennas = defaultdict(set)
    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            if c != '.':
                antennas[c].add(i + j * 1j)
    return antennas, len(lines[0]), len(lines)


def find_antinodes(positions: set[complex], max_x, max_y):
    antinodes = set()
    for a, b in itertools.combinations(positions, 2):
        distance = a - b
        from_a = a + distance
        from_b = b - distance
        if 0 <= from_a.real < max_x and 0 <= from_a.imag < max_y:
            antinodes.add(from_a)
        if 0 <= from_b.real < max_x and 0 <= from_b.imag < max_y:
            antinodes.add(from_b)
    return antinodes


def find_all_antinodes(positions: set[complex], max_x, max_y):
    antinodes = set()
    for a, b in itertools.combinations(positions, 2):
        distance = a - b
        temp = a
        while 0 <= temp.real < max_x and 0 <= temp.imag < max_y:
            antinodes.add(temp)
            temp -= distance
        temp = a + distance
        while 0 <= temp.real < max_x and 0 <= temp.imag < max_y:
            antinodes.add(temp)
            temp += distance
    return antinodes


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "08.txt") as f:
        antennas, max_x, max_y= lines_to_grid(f.read().splitlines())
        antinodes = set()
        all_antinodes = set()
        for _, positions in antennas.items():
            antinodes.update(find_antinodes(positions, max_x, max_y))
            all_antinodes.update(find_all_antinodes(positions, max_x, max_y))
        print_day(8, len(antinodes), len(all_antinodes))


if __name__ == '__main__':
    main()
