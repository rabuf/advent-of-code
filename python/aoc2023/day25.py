import sys
from functools import reduce
from operator import mul, and_
from pathlib import Path

import networkx
from networkx import Graph, condensation, enumerate_all_cliques, is_k_edge_connected, bridges, edge_connectivity, \
    k_edge_components, k_edge_subgraphs, connected_components, set_edge_attributes
from networkx.algorithms.approximation import min_edge_dominating_set, min_weighted_dominating_set
from networkx.algorithms.community import greedy_modularity_communities

from aoc_util import print_day


def parse_line(line):
    name, neighbors = line.split(':')
    neighbors = neighbors.strip().split()
    return name, neighbors


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "25.txt") as f:
        lines = map(parse_line, f.read().splitlines())
        graph = networkx.to_networkx_graph(dict(lines))
        communities = greedy_modularity_communities(graph, cutoff=2, best_n=2)
        p1 = reduce(mul, map(len, communities))
        print_day(25, p1, "FREEBIE!!")


if __name__ == '__main__':
    main()
