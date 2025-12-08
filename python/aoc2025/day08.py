import heapq
import sys
from functools import reduce
from operator import mul
from pathlib import Path

import networkx as nx

from aoc_util import print_day

YEAR = "2025"
DAY = "08"


def parse_line(line):
    return tuple(map(int, line.split(",")))


def all_pairs_distances(junction_boxes):
    ordered = []
    for i, (ax, ay, az) in enumerate(junction_boxes):
        for bx, by, bz in junction_boxes[i + 1 :]:
            a = (ax, ay, az)
            b = (bx, by, bz)
            distance = ((ax - bx) ** 2 + (ay - by) ** 2 + (az - bz) ** 2) ** (1 / 2)
            heapq.heappush(ordered, (distance, a, b))
    return ordered


def part1(boxes):
    ordered = all_pairs_distances(boxes)
    graph = nx.Graph()

    for box in boxes:
        graph.add_node(box)

    for i in range(1000):
        _, a, b = heapq.heappop(ordered)
        graph.add_edge(a, b)
    top3 = sorted((len(s) for s in nx.connected_components(graph)), reverse=True)[:3]

    return reduce(mul, top3)


def part2(boxes):
    ordered = all_pairs_distances(boxes)
    G = nx.Graph()

    for box in boxes:
        G.add_node(box)

    while not nx.is_connected(G):
        _, a, b = heapq.heappop(ordered)
        while nx.has_path(G, a, b):
            G.add_edge(a, b)
            _, a, b = heapq.heappop(ordered)
        G.add_edge(a, b)
    return a[0] * b[0]


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / YEAR / f"{DAY}.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        print_day(DAY, part1(lines), part2(lines))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
