import re
import sys
from itertools import combinations
from pathlib import Path

from networkx import DiGraph, topological_sort
from aoc_util import print_day
import shapely


def parse_line(line):
    [xmin, ymin, zmin, xmax, ymax, zmax] = [int(n) for n in re.split(r'[~,]', line)]
    shape = shapely.Point(xmin, ymin)
    if xmin != xmax:
        shape = shapely.union_all([shapely.Point(xmin + dx, ymin) for dx in range(xmax - xmin + 1)])
    if ymin != ymax:
        shape = shapely.union_all([shapely.Point(xmin, ymin + dy) for dy in range(ymax - ymin + 1)])
    return shape, min(zmin, zmax), max(zmin, zmax)


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "22.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        lines = sorted(lines, key=lambda line: line[1])
        print(lines)
        for i, (brick, lz, uz) in enumerate(lines):
            distance = lz - 1
            for blocker, _, blocker_z in lines[:i]:
                if brick & blocker:
                    distance = min(distance, lz - blocker_z - 1)
            lines[i] = brick, lz - distance, uz - distance
        lines = sorted(lines, key=lambda line: line[1])
        print(lines)
        support_graph = DiGraph()
        for i, (node, lz, rz) in enumerate(lines):
            support_graph.add_node(i)
            z = lz
            for j, (predecessor, blz, buz) in enumerate(lines[:i]):
                pz = buz
                if node.intersects(predecessor) and z == pz + 1:
                    support_graph.add_edge(j, i)
        print(support_graph)
        # print(support_graph.reverse())
        # support_graph.reverse(copy=False)
        removable = set()
        for node in support_graph:
            print(lines[node], node, [n for n in support_graph.neighbors(node)])
            if all(support_graph.in_degree(neighbor) > 1 for neighbor in support_graph.neighbors(node)):
                removable.add(node)
        print(removable)
        print_day(22, len(removable), len(lines))


if __name__ == '__main__':
    main()
