import re
import sys
from pathlib import Path

from networkx import DiGraph

from aoc_util import print_day


def parse_line(line):
    [xmin, ymin, zmin, xmax, ymax, zmax] = [int(n) for n in re.split(r"[~,]", line)]
    shape = {(xmin, ymin)}
    if xmin != xmax:
        shape = {(xmin + dx, ymin) for dx in range(xmax - xmin + 1)}
    if ymin != ymax:
        shape = {(xmin, ymin + dy) for dy in range(ymax - ymin + 1)}
    return shape, min(zmin, zmax), max(zmin, zmax)


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "22.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        lines = drop_blocks(lines)
        support_graph = create_support_graph(lines)
        removable = set()
        for node in support_graph:
            if all(
                support_graph.in_degree(neighbor) > 1
                for neighbor in support_graph.neighbors(node)
            ):
                removable.add(node)
        drops = 0
        for node in support_graph:
            if node not in removable:
                drops += drop_count(node, support_graph)
        print_day(22, len(removable), drops)


def drop_count(node, support_graph):
    dropped = support_graph.copy()
    q = [*dropped.neighbors(node)]
    dropped.remove_node(node)
    while q:
        n = q.pop()
        if dropped.in_degree(n) == 0:
            q.extend(dropped.neighbors(n))
            dropped.remove_node(n)
    return len(support_graph) - len(dropped) - 1


def create_support_graph(lines):
    support_graph = DiGraph()
    for i, (node, lz, rz) in enumerate(lines):
        support_graph.add_node(i)
        z = lz
        for j, (predecessor, blz, buz) in enumerate(lines[:i]):
            pz = buz
            if node & predecessor and z == pz + 1:
                support_graph.add_edge(j, i)
    return support_graph


def drop_blocks(lines):
    lines = sorted(lines, key=lambda line: line[1])
    for i, (brick, lz, uz) in enumerate(lines):
        distance = lz - 1
        for blocker, _, blocker_z in lines[:i]:
            if brick & blocker:
                distance = min(distance, lz - blocker_z - 1)
        lines[i] = brick, lz - distance, uz - distance
    lines = sorted(lines, key=lambda line: line[1])
    return lines


if __name__ == "__main__":
    main()
