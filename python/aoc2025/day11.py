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


def part1(data):
    G = nx.DiGraph()
    for source, destinations in data:
        for d in destinations:
            G.add_edge(source, d)
    return len(list(nx.all_simple_paths(G, "you", "out")))


def part2(data):
    G = nx.DiGraph()
    for source, destinations in data:
        for d in destinations:
            G.add_edge(source, d)

    @cache
    def dfs(node, target="out"):
        if node == target:
            return 1
        return reduce(add, (dfs(s, target) for s in G.successors(node)), 0)

    svr_to_fft = dfs("svr", "fft")
    fft_to_dac = dfs("fft", "dac")
    dac_to_out = dfs("dac", "out")
    return svr_to_fft * fft_to_dac * dac_to_out


SAMPLE = """aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
"""

SAMPLE2 = """svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
"""


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / YEAR / f"{DAY}.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        sample1 = list(map(parse_line, SAMPLE.splitlines()))
        sample2 = list(map(parse_line, SAMPLE2.splitlines()))
        print_day(DAY, part1(lines), part2(lines), part2(sample2))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
