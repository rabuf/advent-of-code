import heapq
import sys
from collections import defaultdict
from pathlib import Path

import networkx as nx

from aoc_util import print_day


def parse_line(line):
    return line


def lines_to_graph(lines):
    g = nx.Graph()
    portal_letters = {}
    max_x = 0
    min_x = len(lines[0])
    max_y = 0
    min_y = len(lines)

    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            pos = (i, j)
            match c:
                case ".":
                    max_x = max(max_x, i)
                    max_y = max(max_y, j)
                    min_x = min(min_x, i)
                    min_y = min(min_y, i)
                    g.add_node(pos)
                    for n in (
                        (i + dx, j + dy)
                        for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1))
                    ):
                        if n in g:
                            g.add_edge(pos, n)
                case "#":
                    pass
                case " ":
                    pass
                case _:
                    portal_letters[pos] = c

    inner = {}
    outer = {}
    portals = defaultdict(list)
    for x, y in portal_letters:
        name = ""
        portal = x, y
        pos = x, y
        down = x, y + 1
        right = x + 1, y
        if down in portal_letters:
            name = portal_letters[pos] + portal_letters[down]
            node_down = x, y + 2
            node_up = x, y - 1
            if node_down in g:
                portals[name].append(node_down)
                portal = node_down
            elif node_up in g:
                portals[name].append(node_up)
                portal = node_up
            else:
                raise ValueError(name, portals[name])
        elif right in portal_letters:
            name = portal_letters[pos] + portal_letters[right]
            node_left = x - 1, y
            node_right = x + 2, y
            if node_left in g:
                portals[name].append(node_left)
                portal = node_left
            elif node_right in g:
                portals[name].append(node_right)
                portal = node_right
            else:
                raise ValueError(name, portals[name])
        if min_x < x < max_x and min_y < y < max_y:
            inner[name] = portal
        else:
            outer[name] = portal
    h = g.copy()
    for p, pos in portals.items():
        if p not in ("AA", "ZZ"):
            g.add_edge(*pos)
        assert len(pos) == 2 or p == "AA" or p == "ZZ"
    return g, portals["AA"][0], portals["ZZ"][0], h, inner, outer


def part2(
    g: nx.Graph, inner: dict[str, tuple[int, int]], outer: dict[str, tuple[int, int]]
):
    """
    A fairly standard Dijkstra's algorithm based search. Whenever a node is also a portal, can go up/down a level
    of depth. Target is still 'AA' and 'ZZ'. Fortunately they're given in the `outer` set of nodes.
    """
    outer_portals = {v: k for k, v in outer.items() if k not in ("AA", "ZZ")}
    inner_portals = {v: k for k, v in inner.items()}
    start = 0, outer["AA"]
    target = 0, outer["ZZ"]
    frontier = []
    heapq.heappush(frontier, (0, *start))
    seen = set()
    while frontier:
        distance, depth, pos = heapq.heappop(frontier)
        if (depth, pos) == target:
            return distance
        if (depth, pos) in seen:
            continue
        seen.add((depth, pos))
        for n in g[pos]:
            if (depth, n) not in seen:
                heapq.heappush(frontier, (distance + 1, depth, n))
        if pos in outer_portals and depth > 0:
            n = inner[outer_portals[pos]]
            heapq.heappush(frontier, (distance + 1, depth - 1, n))
        if pos in inner_portals:
            n = outer[inner_portals[pos]]
            heapq.heappush(frontier, (distance + 1, depth + 1, n))


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2019" / "20.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        g, start, end, h, inner, outer = lines_to_graph(lines)
        p1 = nx.shortest_path_length(g, start, end)
        p2 = part2(h, inner, outer)
        print_day("20", p1, p2, len(lines))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
