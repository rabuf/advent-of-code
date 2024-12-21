import math
import sys
from itertools import zip_longest
from pathlib import Path

import networkx

from aoc_util import print_day



def make_graph(keys):
    G = networkx.Graph()
    G.add_nodes_from(v for _, v in keys.items())
    for n in G:
        for d in (n + d for d in (1, -1, 1j, -1j)):
            if d in G:
                G.add_edge(n, d)
    return G


key_to_pos = {
    'A': 0,
    '0': -1,
    '1': -2 + 1j,
    '2': -1 + 1j,
    '3': 1j,
    '4': -2 + 2j,
    '5': -1 + 2j,
    '6': 2j,
    '7': -2 + 3j,
    '8': -1 + 3j,
    '9': 3j,
}

arrow_to_pos = {
    'A': 0,
    '^': -1,
    '<': -2 - 1j,
    'v': -1 - 1j,
    '>': -1j,
}

pos_to_key = {v: k for k, v in key_to_pos.items()}
pos_to_arrow = {v: k for k, v in arrow_to_pos.items()}

key_graph = make_graph(key_to_pos)
arrow_graph = make_graph(arrow_to_pos)

dir_to_arrow = {
    0: 'A',
    -1: '<',
    1: '>',
    -1j: 'v',
    1j: '^',
}

arrow_to_dir = {v: k for k, v in dir_to_arrow.items()}


def recursive_direction_generator(G, buttons, to_pos, to_key, start='A'):
    def recur(pos, bs):
        if bs:
            dest = bs[0]
            options = networkx.all_shortest_paths(G, to_pos[pos], to_pos[dest])
            for o in options:
                for rest in recur(dest, bs[1:]):
                    yield o + rest
        else:
            yield []

    for path in recur(start, buttons):
        yield ''.join(dir_to_arrow[b - a] for a, b in zip(path, path[1:])) + 'A'


def directions(G, buttons, to_pos, pos='A'):
    result = []
    for b in buttons:
        steps = networkx.shortest_path(G, to_pos[pos], to_pos[b])
        for m, n in zip(steps, steps[1:]):
            d = n - m
            result.append(dir_to_arrow[d])
        result.append('A')
        pos = b
    return ''.join(result)


def apply_arrows(G, directions, to_name):
    pos = 0
    result = []
    for d in directions:
        if d == 'A':
            result.append(to_name[pos])
        else:
            pos += arrow_to_dir[d]
    return ''.join(result)


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "21.txt") as f:
            lines = f.read().splitlines()

        p1 = 0
        for digits in lines:
            part_min = part1(digits)
            n = int(digits[:-1])
            p1 += part_min * n
            print(p1)

        print_day("21", p1, 2, len(lines))
    except IOError as e:
        print(e)


def part1(digits):
    part_min = 0
    for a, b in zip('A' + digits, digits):
        r3_min = math.inf
        for r1 in recursive_direction_generator(key_graph, b, key_to_pos, pos_to_arrow, a):
            for r2 in recursive_direction_generator(arrow_graph, r1, arrow_to_pos, pos_to_arrow):
                for r3 in recursive_direction_generator(arrow_graph, r2, arrow_to_pos, pos_to_arrow):
                    r3_min = min(len(r3), r3_min)
                    if len(r3) >= r3_min: break
        part_min += r3_min
    return part_min


if __name__ == '__main__':
    main()
