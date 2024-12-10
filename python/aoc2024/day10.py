import sys
from collections import defaultdict
from pathlib import Path

from aoc_util import print_day


def lines_to_map(lines):
    map_grid = defaultdict(int)
    trailheads = set()
    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            height = int(c)
            position = i + j * 1j
            map_grid[position] = height
            if height == 0:
                trailheads.add(position)
    return map_grid, trailheads


def score(trailhead, map_grid):
    frontier = {trailhead}
    peaks = set()
    while frontier:
        position = frontier.pop()
        height = map_grid[position]
        if height < 9:
            neighbors = (position + direction for direction in (1, -1, 1j, -1j))
            for n in neighbors:
                if map_grid[n] == height + 1:
                    frontier.add(n)
        else:
            peaks.add(position)
    return len(peaks)


def rating(trailhead, map_grid):
    frontier = {tuple([trailhead])}
    trails = set()
    while frontier:
        path = frontier.pop()
        position = path[0]
        height = map_grid[position]
        if height < 9:
            neighbors = (position + direction for direction in (1, -1, 1j, -1j))
            for n in neighbors:
                if map_grid[n] == height + 1:
                    frontier.add(tuple([n, *path]))
        else:
            trails.add(path)
    return len(trails)


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "10.txt") as f:
        map_grid, trailheads = lines_to_map(f.read().splitlines())
    p1 = sum(score(trailhead, map_grid) for trailhead in trailheads)
    p2 = sum(rating(trailhead, map_grid) for trailhead in trailheads)
    print_day(10, p1, p2)


if __name__ == '__main__':
    main()
