import math
import sys
from itertools import zip_longest
from functools import cache
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


@cache
def optimal_single(start, end, depth):
    if depth == 0:
        return optimal_directions(pos_to_key[start], key_to_pos, pos_to_key[end])


def optimal_directions(buttons, to_pos, start='A'):
    path = []
    for a, b in zip(start + buttons, buttons):
        distance = to_pos[b] - to_pos[a]
        x, y = int(distance.real), int(distance.imag)
        xs = (x > 0) - (x < 0)
        ys = ((0 < y) - (y < 0)) * 1j
        match x, y:
            case 0, 0:
                pass
            case x, 0:
                path.append(dir_to_arrow[xs] * abs(x))
            case 0, y:
                path.append(dir_to_arrow[ys] * abs(y))
            case x, y if to_pos[a] + x == -2:
                path.append(dir_to_arrow[ys] * abs(y) + dir_to_arrow[xs] * abs(x))
            case x, y if to_pos[a] + y * 1j == -2:
                path.append(dir_to_arrow[xs] * abs(x) + dir_to_arrow[ys] * abs(y))
            case x, y if x < 0:
                path.append(dir_to_arrow[xs] * abs(x) + dir_to_arrow[ys] * abs(y))
            case x, y:
                path.append(dir_to_arrow[ys] * abs(y) + dir_to_arrow[xs] * abs(x))
        path.append('A')
    return ''.join(path)


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


def apply_arrows(directions, to_name):
    pos = 0
    result = []
    for d in directions:
        if pos not in to_name: raise ValueError(pos)
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
            r = optimal_directions(digits, key_to_pos)
            for _ in range(2):
                r = optimal_directions(r, arrow_to_pos)
            n = int(digits[:-1])
            p1 += n * len(r)
        p2 = 0
        # This grows way too big, what's a better way?
        # for digits in lines:
        #     r = optimal_directions(digits[0], key_to_pos)
        #     for _ in range(13):
        #         r = optimal_directions(r, arrow_to_pos)
        #     n = int(digits[:-1])
        #     p2 += n * len(r)
        print_day("21", p1, p2)
        assert p1 == 163086
    except IOError as e:
        print(e)


def part1(digits):
    part_min = 0
    for a, b in zip('A' + digits, digits):
        r3_min = math.inf
        for r1 in optimal_directions(key_graph, b, key_to_pos, pos_to_arrow, a):
            for r2 in optimal_directions(arrow_graph, r1, arrow_to_pos, pos_to_arrow):
                for r3 in optimal_directions(arrow_graph, r2, arrow_to_pos, pos_to_arrow):
                    r3_min = min(len(r3), r3_min)
                    if len(r3) >= r3_min: break
        part_min += r3_min
    return part_min


if __name__ == '__main__':
    main()
