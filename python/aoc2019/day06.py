import sys
from pathlib import Path

from aoc_util import print_day
import networkx as nx

def parse_line(line):
    return line.split(')')


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "06.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        D = nx.DiGraph()
        D.add_edges_from(lines)
        sp = dict(nx.all_pairs_shortest_path(D))
        p1 = sum(len(v) for _, v in sp.items()) - len(sp)
        G = nx.Graph()
        G.add_edges_from(lines)
        p2 = nx.shortest_path(G, 'YOU', 'SAN')
        print_day("06", p1, len(p2) - 3)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
