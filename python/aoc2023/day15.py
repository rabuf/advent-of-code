import re
import sys
from collections import defaultdict
from pathlib import Path

from aoc_util import print_day


def hash_algorithm(s):
    h = 0
    for c in s:
        h += ord(c)
        h *= 17
        h %= 256
    return h


def parse_line(line):
    pass


def process(instruction):
    label, operation, n = re.match(r'(\w+)([=-])(\d+)?', instruction).groups(0)
    return label, operation, int(n)


def focusing_power(hashmap):
    total = 0
    for i, box in hashmap.items():
        for j, [_, focal_length] in enumerate(box[1], start=1):
            total += (i + 1) * j * focal_length
    return total


def hash_map(instructions):
    m = defaultdict(lambda: (set(), []))
    for instruction in instructions:
        match process(instruction):
            case label, '=', n:
                box = m[hash_algorithm(label)]
                if label in box[0]:
                    for entry in box[1]:
                        if entry[0] == label:
                            entry[1] = n
                else:
                    box[0].add(label)
                    box[1].append([label, n])
            case label, '-', _:
                box = m[hash_algorithm(label)]
                if label in box[0]:
                    box[0].remove(label)
                    for i, entry in enumerate(box[1]):
                        if entry[0] == label:
                            break
                    if i < len(box[1]):
                        box[1].remove(entry)
    return m


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "15.txt") as f:
        instructions = f.readline().strip().split(',')
        m = hash_map(instructions)
        p2 = focusing_power(m)
        print_day(15, sum(hash_algorithm(i) for i in instructions), p2)


if __name__ == '__main__':
    main()
