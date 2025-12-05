import sys
from pathlib import Path

import networkx
from networkx.algorithms.community import kernighan_lin_bisection

from aoc_util import print_day


def parse_line(line):
    name, neighbors = line.split(":")
    neighbors = neighbors.strip().split()
    return name, neighbors


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "25.txt") as f:
        lines = map(parse_line, f.read().splitlines())
        graph = networkx.to_networkx_graph(dict(lines))
        p1, p2 = kernighan_lin_bisection(graph)
        p1 = len(p1) * len(p2)
        print_day(25, p1, "FREEBIE!!")


if __name__ == "__main__":
    main()
