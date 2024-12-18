import math
import sys
from collections import defaultdict
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    directions = {'R': 1, 'L': -1, 'U': 1j, 'D': -1j}
    steps = [(directions[p[0]], int(p[1:])) for p in line.split(',')]
    return steps


def process(instructions):
    visited = defaultdict(lambda: math.inf)
    position = 0
    steps = 0
    for d, distance in instructions:
        visited.update((position + d * n, min(visited[position + d * n], steps + n)) for n in range(1, distance + 1))
        steps = steps + distance
        position = position + d * distance
    return visited


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "03.txt") as f:
            s1, s2 = list(map(parse_line, f.read().splitlines()))
            r1, r2 = process(s1), process(s2)
            p1 = math.inf
            p2 = math.inf
            for pos in set(r1).intersection(r2):
                p1 = int(min(p1, abs(pos.real) + abs(pos.imag)))
                p2 = min(p2, r1[pos] + r2[pos])
            print_day("03", p1, p2, len(s2), len(s2))
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
