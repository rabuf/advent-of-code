import heapq
import sys
from collections import defaultdict
from itertools import combinations
from pathlib import Path

import networkx as nx

from aoc_util import print_day


def solve(G: nx.Graph, doors: dict, keys: dict, start: list[tuple[int, int]]):
    key_to_pos = {v: k for k, v in keys.items()}
    door_to_pos = {v: k for k, v in doors.items()}

    paths = defaultdict(dict)
    distances = defaultdict(dict)
    for a, b in combinations(keys, 2):
        if nx.has_path(G, a, b):
            paths[a][b] = nx.shortest_path(G, a, b)
            paths[b][a] = nx.shortest_path(G, b, a)
            distances[a][b] = distances[b][a] = len(paths[a][b]) - 1
    for s in start:
        for a in keys:
            if nx.has_path(G, s, a):
                paths[s][a] = nx.shortest_path(G, s, a)
                distances[s][a] = len(paths[s][a]) - 1

    def reachable_keys(p, h):
        nonlocal key_to_pos, door_to_pos
        targets = set(key_to_pos)
        targets.difference_update(h)
        locked = {door_to_pos[k.upper()] for k in targets if k.upper() in door_to_pos}
        reachable = set()
        for t in targets:
            if key_to_pos[t] in paths[p]:
                path = paths[p][key_to_pos[t]]
                if not locked.intersection(path):
                    reachable.add(t)
        return reachable

    seen = set()
    frontier = []
    heapq.heappush(frontier, (0, tuple(), *start))
    target = tuple(sorted(keys.values()))
    while frontier:
        distance, held, *pos = heapq.heappop(frontier)
        if held == target:
            return distance
        if (held, *pos) in seen:
            continue
        seen.add((held, *pos))

        for i, p in enumerate(pos):
            reachable = reachable_keys(p, held)
            for n in reachable:
                n_pos = key_to_pos[n]
                positions = pos[:i] + [n_pos] + pos[i + 1 :]
                d = distances[p][n_pos]
                n_held = tuple(sorted((n, *held)))
                if (n_held, *positions) in seen:
                    continue
                heapq.heappush(frontier, (distance + d, n_held, *positions))


def lines_to_graph(lines: list[str]):
    G = nx.Graph()
    doors = dict()
    keys = dict()
    start = 0
    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            p = (i, j)
            match c:
                case "#":
                    continue
                case "@":
                    start = p
                case _ if c.isupper():
                    doors[p] = c
                case _ if c.islower():
                    keys[p] = c
            G.add_node(p)
            for dx, dy in ((-1, 0), (1, 0), (0, 1), (0, -1)):
                n = (i + dx, j + dy)
                if n in G:
                    G.add_edge(n, p)
    return G, doors, keys, start


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2019" / "18.txt") as f:
            lines = list(f.read().splitlines())
        G, doors, keys, start = lines_to_graph(lines)
        p1 = solve(G, doors, keys, [start])
        print(p1)
        P2 = G.copy()
        x, y = start
        for p in [
            (x + dx, y + dy) for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1), (0, 0))
        ]:
            P2.remove_node(p)
        starts = [(x + dx, y + dy) for (dx, dy) in ((-1, -1), (1, 1), (-1, 1), (1, -1))]
        p2 = solve(P2, doors, keys, starts)
        print_day("18", p1, p2)
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
