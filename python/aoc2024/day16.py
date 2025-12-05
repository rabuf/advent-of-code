import heapq
import math
import sys
from collections import defaultdict
from pathlib import Path

from aoc_util import print_day


def lines_to_grid(lines):
    start = 0
    end = 0
    grid = {}
    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            pos = (i, j)
            if c == "S":
                start = pos
                grid[pos] = "."
            elif c == "E":
                end = pos
                grid[pos] = "."
            else:
                grid[pos] = c
    return grid, start, end


def dijkstra(grid, start, directions):
    q = [(0, start, d) for d in directions]
    cost = defaultdict(lambda: math.inf)
    visited = set()
    while q:
        score, pos, d = heapq.heappop(q)
        if (pos, d) in visited:
            continue
        visited.add((pos, d))
        cost[(pos, d)] = min(cost[(pos, d)], score)
        turns = (-d[1], -d[0]), (d[1], d[0])
        n = (pos[0] + d[0], pos[1] + d[1])
        if grid[n] == ".":
            heapq.heappush(q, (score + 1, n, d))
        for turn in turns:
            heapq.heappush(q, (score + 1000, pos, turn))
    return cost


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2024" / "16.txt") as f:
            lines = f.read().splitlines()
        grid, start, end = lines_to_grid(lines)
        directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]
        forward_cost = dijkstra(grid, start, [(1, 0)])
        backward_cost = dijkstra(grid, end, directions)
        p1 = min(forward_cost[(end, d)] for d in directions)
        p2 = len(
            {
                pos
                for (pos, (dx, dy)), cost in backward_cost.items()
                if forward_cost[(pos, (-dx, -dy))] + cost == p1
            }
        )
        print_day("16", p1, p2)
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
