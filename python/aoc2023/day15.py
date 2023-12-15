import re
import sys
from collections import defaultdict
from functools import reduce
from pathlib import Path

from aoc_util import print_day


def hash_algorithm(s):
    return reduce(lambda h, c: (h + ord(c)) * 17 % 256, s, 0)


def process(instruction):
    label, operation, n = re.match(r'(\w+)([=-])(\d+)?', instruction).groups(0)
    return label, operation, int(n)


def focusing_power(hashmap):
    total = 0
    for i, box in hashmap.items():
        for j, label in enumerate(box[1], start=1):
            total += (i + 1) * j * box[0][label]
    return total


def apply(hashmap: dict[int, tuple[dict, list]], label, operation, n):
    box = hashmap[hash_algorithm(label)]
    match operation:
        case '=':
            if label not in box[0]:
                box[1].append(label)
            box[0][label] = n
        case '-':
            if label in box[0]:
                del box[0][label]
                box[1].remove(label)
    return hashmap


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "15.txt") as f:
        raw_instructions = f.readline().strip().split(',')
        p1 = sum(hash_algorithm(i) for i in raw_instructions)
        instructions = [process(instruction) for instruction in raw_instructions]
        m = defaultdict(lambda: ({}, []))
        reduce(lambda m, instruction: apply(m, *instruction), instructions, m)
        p2 = focusing_power(m)
        print_day(15, p1, p2)


if __name__ == '__main__':
    main()
