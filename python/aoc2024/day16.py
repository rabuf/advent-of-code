import sys
from pathlib import Path
from queue import PriorityQueue

from aoc_util import print_day


def lines_to_grid(lines):
    start = 0
    end = 0
    grid = {}
    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            pos = (i, j)
            if c == 'S':
                start = pos
                grid[pos] = '.'
            elif c == 'E':
                end = pos
                grid[pos] = '.'
            else:
                grid[pos] = c
    return grid, start, end


def reindeer_race(grid, start, end):
    q = PriorityQueue()
    q.put((0, start, (1, 0)))
    visited = set()
    while q:
        score, pos, d = q.get()
        if (pos, d) in visited:
            continue
        visited.add((pos, d))
        left, right = (-d[1], -d[0]), (d[1], d[0])
        if pos == end:
            return score
        if grid[(pos[0] + d[0], pos[1] + d[1])] == '.':
            q.put((score + 1, (pos[0] + d[0], pos[1] + d[1]), d))
        for turn in (left, right):
            if grid[(pos[0] + turn[0], pos[1] + turn[1])] == '.':
                q.put((score + 1000, pos, turn))


def best_seats_dfs(grid, start, end, limit):
    def recur(score, pos, d, visited):
        visited = visited | {(pos, d)}
        if pos == end:
            print(len(visited))
            return visited
        left, right = (-d[1], -d[0]), (d[1], d[0])
        result = set()
        n = (pos[0] + d[0], pos[1] + d[1])
        if grid[n] == '.' and score + 1 <= limit and (n, d) not in visited:
            result.update(recur(score + 1, n, d, visited))
        for turn in (left, right):
            n = (pos[0] + turn[0], pos[1] + turn[1])
            if grid[n] == '.' and score + 1000 <= limit and (pos, turn) not in visited:
                result.update(recur(score + 1000, pos, turn, visited))
        return result
    return recur(0, start, (1, 0), set())


def best_seats(grid, start, end, limit):
    q = []
    q.append((0, start, (1, 0), (start,)))
    visited = set()
    seats = set()
    while q:
        score, pos, d, path = q.pop(-1)
        if score > limit:
            continue
        if (d, path) in visited:
            continue
        visited.add((d, path))
        left, right = (-d[1], -d[0]), (d[1], d[0])
        if pos == end:
            print('found one')
            seats.update(path)
            continue
        n = (pos[0] + d[0], pos[1] + d[1])
        if grid[n] == '.':
            q.append((score + 1, n, d, (n, *path)))
        for turn in (left, right):
            if grid[(pos[0] + turn[0], pos[1] + turn[1])] == '.':
                q.append((score + 1000, pos, turn, path))
    return len(seats)


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "16.txt") as f:
            lines = f.read().splitlines()
        grid, start, end = lines_to_grid(lines)
        p1 = reindeer_race(grid, start, end)
        p2 = best_seats(grid, start, end, p1)
        print_day("16", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
