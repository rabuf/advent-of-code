import sys
from functools import cache, reduce
from operator import add
from pathlib import Path

import networkx as nx

from aoc_util import print_day

YEAR = "2025"
DAY = "11"


def parse_line(line):
    source, destinations = line.split(":")
    destinations = destinations.strip().split()
    return source, destinations


def make_graph(data):
    G = nx.DiGraph()
    for source, destinations in data:
        for d in destinations:
            G.add_edge(source, d)
    return G


def part1(G):
    return dfs(G, "you", "out")


def dfs(G, node, target):
    @cache
    def wrapped(node):
        if node == target:
            return 1
        return sum(wrapped(s) for s in G.successors(node))

    return wrapped(node)


def part2(G):
    if nx.has_path(G, "fft", "dac"):
        svr_to_fft = dfs(G, "svr", "fft")
        fft_to_dac = dfs(G, "fft", "dac")
        dac_to_out = dfs(G, "dac", "out")
        return svr_to_fft * fft_to_dac * dac_to_out
    else:
        svr_to_dac = dfs(G, "svr", "dac")
        dac_to_fft = dfs(G, "dac", "fft")
        fft_to_out = dfs(G, "fft", "out")
        return svr_to_dac * dac_to_fft * fft_to_out


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / YEAR / f"{DAY}.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        G = make_graph(lines)

        print_day(DAY, part1(G), part2(G))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
