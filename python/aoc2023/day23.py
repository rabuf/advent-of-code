import sys
from pathlib import Path

from networkx import DiGraph, Graph, all_simple_paths, path_weight

from aoc_util import print_day


def create_graph(lines, climbable=False):
    graph = DiGraph() if not climbable else Graph()
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            pos = x + y * len(lines[0])
            if c == "#":
                continue
            graph.add_node(pos, kind=c)
    nodes = dict(graph.nodes(data="kind"))
    for pos in nodes:
        neighbors = []
        if climbable or nodes[pos] == ".":
            neighbors = [
                pos + delta for delta in [1, -1, len(lines[0]), -len(lines[0])]
            ]
        if not climbable:
            match nodes[pos]:
                case "<":
                    neighbors = [pos - 1]
                case ">":
                    neighbors = [pos + 1]
                case "v":
                    neighbors = [pos + len(lines[0])]
                case "^":
                    neighbors = [pos - len(lines[0])]
        for n in neighbors:
            if n in nodes:
                graph.add_edge(pos, n, weight=1)
    return graph


def parse_line(line):
    return line


def compress(graph):
    curr = graph.copy()
    removed = True
    while removed:
        removed = False
        for n in curr.nodes:
            if curr.degree(n) == 2:
                removed = True
                neighbors = list(curr.neighbors(n))
                left, right = neighbors
                lw = curr.get_edge_data(left, n)
                rw = curr.get_edge_data(right, n)
                curr.add_edge(
                    neighbors[0], neighbors[1], weight=lw["weight"] + rw["weight"]
                )
                curr.remove_node(n)
                break
    return curr


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "23.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        graph = create_graph(lines)
        start = 1
        end = -2 + len(lines) * len(lines[0])
        climbing_graph = create_graph(lines, climbable=True)
        cg = compress(climbing_graph)
        p1 = max(
            path_weight(graph, path, "weight")
            for path in all_simple_paths(graph, start, end)
        )
        p2 = max(
            path_weight(cg, path, "weight") for path in all_simple_paths(cg, start, end)
        )
        print_day(23, p1, p2)


if __name__ == "__main__":
    main()
