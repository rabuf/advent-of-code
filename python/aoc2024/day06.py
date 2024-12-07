import sys
from collections import defaultdict
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return line


def lines_to_grid(lines):
    grid = {}
    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            grid[i + j * 1j] = c
            if c == '^':
                start = i + j * 1j
    return grid, start


def loops(grid, position, direction, visited):
    while (position + direction) in grid:
        if (position, direction) in visited:
            return True
        visited.add((position, direction))
        if grid[position + direction] == '#':
            direction *= 1j
        else:
            position += direction
    return False


def looping_obstruction_count(grid, position):
    direction = -1j
    obstacles = 0
    visited = set()
    moves = set()
    while position in grid:
        next_position = position + direction
        visited.add(position)
        if next_position not in grid: break
        moves.add((position, direction))
        match grid[next_position]:
            case '#':
                direction *= 1j
            case '.' if next_position not in visited:
                grid[next_position] = '#'
                if loops(grid, position, direction, moves - {(position, direction)}):
                    obstacles += 1
                grid[next_position] = '.'
                position += direction
            case _:
                position += direction
    return len(visited), obstacles


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "06.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        grid, start = lines_to_grid(lines)
        p1, p2 = looping_obstruction_count(grid, start)
        print_day(6, p1, p2)


if __name__ == '__main__':
    main()
