import sys
from itertools import combinations
from pathlib import Path

from aoc_util import print_day
import networkx


def parse_line(line):
    return line


def part2(G, walls, start, end):
    cheats = set()
    W = networkx.Graph()
    W.add_nodes_from(walls)
    for node in W.nodes:
        for n in (node + d for d in (1, -1, 1j, -1j)):
            if n in W.nodes:
                W.add_edge(node, n)
    return len(cheats)


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "20.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        G = networkx.Graph()
        walls = []
        start = end = 0
        for j, line in enumerate(lines):
            for i, c in enumerate(line):
                pos = i + j * 1j
                match c:
                    case 'S':
                        start = pos
                        G.add_node(pos)
                    case 'E':
                        end = pos
                        G.add_node(pos)
                    case '.':
                        G.add_node(pos)
                    case _:
                        walls.append(pos)
        for node in G.nodes:
            for n in (node + d for d in (1, -1, 1j, -1j) if node + d in G):
                G.add_edge(node, n)
        from_start = networkx.single_source_shortest_path_length(G, start)
        from_end = networkx.single_source_shortest_path_length(G, end)
        limit = from_start[end]
        p1 = 0
        for w in walls:
            neighbors = [w + d for d in (1, -1, 1j, -1j) if w + d in G]
            if len(neighbors) >= 2:
                for a, b in zip(neighbors, neighbors[1:]):
                    distance = min(from_start[a] + from_end[b], from_start[b] + from_end[a]) + 1
                    if limit - distance >= 100:
                        p1 += 1

        print_day("20", p1, G, len(lines))
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
