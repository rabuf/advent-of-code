import re
import sys
from pathlib import Path

from aoc_util import print_day


def find_parts(schematic: list[str]):
    padding = '.' * len(schematic[0])
    padded = [padding] + schematic
    parts = []
    for i in range(len(schematic)):
        for match in re.finditer(r'\d+', schematic[i]):
            start, end = match.span()
            start, end = max(start - 1, 0), min(end + 1, len(schematic[i]))
            region = ''.join([row[start:end] for row in padded[i:i + 3]])
            if re.search(r'[^.0-9]', region):
                parts.append(int(match.group(0)))
    return parts


def find_gear_ratios(schematic):
    padding = '.' * len(schematic[0])
    padded = [padding] + schematic
    gear_ratios = []
    for i in range(len(schematic)):
        for gear in re.finditer(r'\*', schematic[i]):
            numbers = []
            for row in padded[i:i + 3]:
                for n in re.finditer(r'\d+', row):
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
