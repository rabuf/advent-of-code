import sys
from pathlib import Path

import networkx

from aoc_util import print_day


def parse_line(line):
    return line


def manhattan(a, b):
    return int(abs(a.imag - b.imag) + abs(a.real - b.real))


def offsets(n):
    result = set()
    for i in range(n + 1):
        for j in range(n + 1 - i):
            result.add(complex(i, j))
            result.add(complex(i, -j))
            result.add(complex(-i, j))
            result.add(complex(-i, -j))
    return result - {0}


def cheats(G, start, limit=20, threshold=50):
    cheat_count = 0
    distances = networkx.single_source_shortest_path_length(G, start)
    for a in G:
        for b in (
            b
            for o in offsets(limit)
            if (b := a + o) in G and distances[b] - distances[a] >= threshold
        ):
            m = abs(a.imag - b.imag) + abs(a.real - b.real)
            if distances[b] - distances[a] >= threshold + m:
                cheat_count = cheat_count + 1
    return cheat_count


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2024" / "20.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        G = networkx.Graph()
        start = 0
        for j, line in enumerate(lines):
            for i, c in enumerate(line):
                pos = complex(i, j)
                match c:
                    case "S":
                        start = pos
                        G.add_node(pos)
                    case "." | "E":
                        G.add_node(pos)
                    case _:
                        pass
        for node in G.nodes:
            for n in (node + d for d in (1, -1, 1j, -1j) if node + d in G):
                G.add_edge(node, n)
        p1 = cheats(G, start, limit=2, threshold=100)
        p2 = cheats(G, start, limit=20, threshold=100)
        print_day("20", p1, p2)
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
