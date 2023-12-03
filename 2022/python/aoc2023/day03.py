import sys
from pathlib import Path
import re as re
from aoc_util import print_day


def find_parts(schematic: list[str]):
    padding = '.' * len(schematic[0])
    padded = schematic + [padding]
    parts = []
    for i in range(len(schematic)):
        line = schematic[i]
        for match in re.finditer(r'(\d+)', line):
            start, end = match.span()
            start, end = max(start - 1, 0), min(end + 1, len(line))
            region = [padded[row][start:end] for row in [i-1, i, i+1]]
            region = ''.join(region)
            if re.search(r'[^.0-9]', region):
                parts.append(int(match.group(0)))
    return parts


def find_gear_ratios(schematic):
    padding = '.' * len(schematic[0])
    padded = schematic + [padding]
    gear_ratios = []
    for i in range(len(schematic)):
        for gear in re.finditer(r'\*', schematic[i]):
            numbers = []
            for row in [i-1, i, i+1]:
                for n in re.finditer(r'(\d+)', padded[row]):
                    lower, upper = n.span()
                    if lower - 1 <= gear.start() <= upper:
                        numbers.append(int(n.group(0)))
            if len(numbers) == 2:
                gear_ratios.append(numbers[0] * numbers[1])
    return gear_ratios


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "03.txt") as f:
        lines = f.read().splitlines()
        print_day(3, sum(find_parts(lines)), sum(find_gear_ratios(lines)))


if __name__ == '__main__':
    main()
