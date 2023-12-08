import re
import sys
from itertools import cycle
from math import lcm
from pathlib import Path

from aoc_util import print_day


def parse_node(node_line):
    return re.findall(r'\w+', node_line)


def walk_map(directions, network, start='AAA', limit=lambda pos: pos == 'ZZZ'):
    directions = cycle(directions)
    current = start
    count = 0
    while not limit(current):
        count += 1
        match next(directions):
            case 'L':
                current = network[current][0]
            case 'R':
                current = network[current][1]
            case _:
                raise ValueError(f'Invalid directions')
    return count


def ghost_walk(directions, network):
    positions = [location for location in network if location.endswith('A')]
    print(positions)

    def limit(pos):
        return pos.endswith('Z')

    steps = [walk_map(directions, network, start=position, limit=limit) for position in positions]
    print(steps)
    return lcm(*steps)


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "08.txt") as f:
        lines = f.read().splitlines()
        directions = lines[0]
        network = {name: (left, right) for [name, left, right] in map(parse_node, lines[2:])}
        steps = walk_map(directions, network)
        ghost_steps = ghost_walk(directions, network)
        print_day(8, steps, ghost_steps)


if __name__ == '__main__':
    main()
