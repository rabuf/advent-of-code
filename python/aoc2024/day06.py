import sys
from collections import defaultdict
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return line


def lines_to_grid(lines):
    grid = defaultdict(str)
    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            grid[i + j * 1j] = c
            if c == '^':
                start = i + j*1j
    return grid, start


def loops(grid, start, direction=-1j):
    visited = set()
    position = start
    while grid[position + direction] != '':
        if (position, direction) in visited:
            return True
        visited.add((position, direction))
        if grid[position + direction] == '#':
            direction *= 1j
        else:
            position += direction
    return False


def looping_obstruction_count(grid, start):
    position = start
    direction = -1j
    obstacles = set()
    visited = {start}
    while grid[position] != '':
        next_position = position + direction
        visited.add(position)
        match grid[next_position]:
            case '#':
                direction *= 1j
            case '.':
                if next_position not in visited and loops(grid | {next_position: '#'}, position, direction):
                    obstacles.add(next_position)
                position += direction
            case _:
                position += direction
    return len(visited), len(obstacles)


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "06.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        grid, start = lines_to_grid(lines)
        p1, p2 = looping_obstruction_count(grid, start)
        print_day(6, p1, p2)


if __name__ == '__main__':
    main()
