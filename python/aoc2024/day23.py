import sys
from pathlib import Path

import networkx as nx

from aoc_util import print_day


def parse_line(line):
    return line.split('-')


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "23.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        G = nx.Graph()
        G.add_edges_from(lines)
        p1 = len([c for c in nx.enumerate_all_cliques(G) if len(c) == 3 and any(n[0] == 't' for n in c)])
        p2 = max(nx.find_cliques(G), key=len)
        p2 = ','.join(sorted(p2))
        print_day("23", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
