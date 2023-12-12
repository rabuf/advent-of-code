import datetime
import sys
from pathlib import Path
from functools import cache

from aoc_util import print_day


def configuration_count(row: str, groups) -> int:
    def valid_group(start, length):
        valid = row[start:start + length].count('.') == 0
        valid = valid and len(row[start:start + length]) == length
        valid = valid and (start + length >= len(row) or row[start + length] in '.?')
        return valid

    @cache
    def recur(position, group_id):
        if group_id == len(groups):
            return row[position:].count('#') == 0
        if position >= len(row):
            return 0
        if row[position] == '#':
            if valid_group(position, groups[group_id]):
                return recur(position + groups[group_id] + 1, group_id + 1)
            else:
                return 0
        if valid_group(position, groups[group_id]):
            return (recur(position + groups[group_id] + 1, group_id + 1)
                    + recur(position + 1, group_id))
        return recur(position + 1, group_id)
    return recur(0, 0)


def parse_line(line):
    row, groups = line.split()
    groups = [int(c) for c in groups.split(',')]
    return row, groups


def unfold(record):
    row, groups = record
    return '?'.join([row]*5), groups * 5


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "12.txt") as f:
        records = list(map(parse_line, f.read().splitlines()))
        p1 = sum(configuration_count(*record) for record in records)
        p2 = sum(configuration_count(*unfold(record)) for record in records)
        print_day(12, p1, p2)


if __name__ == '__main__':
    main()
