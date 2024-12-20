import sys
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
            for n in (node + d for d in (1, -1, 1j, -1j)):
                if n in G.nodes:
                    G.add_edge(node, n)
        limit = networkx.dijkstra_path_length(G, start, end)
        cheats = 0
        for w in walls:
            if sum(w + d in G.nodes for d in (1, -1, 1j, -1j)) >= 2:
                G.add_node(w)
                for n in (w + d for d in (1, -1, 1j, -1j)):
                    if n in G.nodes:
                        G.add_edge(w, n)
                distance = networkx.dijkstra_path_length(G, start, end)
                if limit - distance >= 100:
                    cheats += 1
                G.remove_node(w)

        print_day("20r", cheats, G, len(lines))
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
