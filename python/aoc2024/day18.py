import heapq
import math
import sys
from collections import defaultdict
from itertools import product
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    x, y = map(int, line.split(','))
    return x, y


def dijkstra(grid, start):
    q = [(0, start)]
    cost = defaultdict(lambda: math.inf)
    visited = set()
    while q:
        score, (x, y) = heapq.heappop(q)
        pos = (x, y)
        if pos in visited:
            continue
        visited.add(pos)
        cost[pos] = min(cost[pos], score)
        for n in ((x + dx, y + dy) for dx, dy in ((1, 0), (-1, 0), (0, -1), (0, 1))):
            if n in grid and grid[n] == '.':
                heapq.heappush(q, (score + 1, n))
    return cost


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "18.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        grid = {(i, j): '.' for i, j in product(range(71), repeat=2)}
        cost = dijkstra(grid | {pos: '#' for pos in lines[:1024]}, (0, 0))
        p1 = cost[(70, 70)]
        low, high = 1024, len(lines)
        while low <= high:
            mid = (high + low) // 2
            cost = dijkstra(grid | {pos: '#' for pos in lines[:mid]}, (0, 0))
            if cost[(70, 70)] < math.inf:
                low = mid + 1
            else:
                high = mid - 1
        print_day("18", p1, ','.join(map(str, lines[mid])))
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
