import sys
from pathlib import Path
from dataclasses import dataclass

import re

from more_itertools import distinct_permutations

from aoc_util import print_day


@dataclass
class Record:
    row: str
    groups: list[int]


def groups_to_regex(groups: list[int]):
    pattern = r'\.*' + r'\.+'.join('#'*c for c in groups) + r'\.*'
    return re.compile(pattern)


def fill_row(row: str, springs):
    for c in springs:
        next_q = row.find('?')
        row = row[:next_q] + c + row[next_q+1:]
    return row


def configuration_count(record: Record) -> int:
    regex = groups_to_regex(record.groups)
    count = 0
    unknowns = record.row.count('?')
    missing_count = sum(record.groups) - record.row.count('#')
    missing_springs = '#'*missing_count + '.'*(unknowns-missing_count)
    for springs in distinct_permutations(missing_springs):
        subbed = fill_row(record.row, springs)
        if regex.match(subbed):
            count += 1

    return count


def parse_line(line):
    row, groups = line.split()
    groups = [int(c) for c in groups.split(',')]
    return Record(row, groups)


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "12.txt") as f:
        records = list(map(parse_line, f.read().splitlines()))
        p1 = sum(configuration_count(record) for record in records)
        print_day(12, p1, len(records))


if __name__ == '__main__':
    main()
