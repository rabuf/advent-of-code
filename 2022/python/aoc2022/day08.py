import sys
from pathlib import Path

from aoc_util import print_day, input_to_grid


def sees_edge_dir(grid, x, y, dx, dy):
    height = grid[x][y]
    i = x + dx
    j = y + dy
    while 0 <= i < len(grid) and 0 <= j < len(grid[0]):
        if height <= grid[i][j]:
            return False
        i += dx
        j += dy
    return True


def sees_edge(grid, i, j):
    return (sees_edge_dir(grid, i, j, 0, 1)
            or sees_edge_dir(grid, i, j, 0, -1)
            or sees_edge_dir(grid, i, j, 1, 0)
            or sees_edge_dir(grid, i, j, -1, 0))


def print_grid(grid):
    for row in grid:
        for c in row:
            print(c, end='')
        print()


def count_visible_trees(grid):
    rows = len(grid)
    cols = len(grid[0])
    count = sum(sees_edge(grid, i, j)
                for i in range(0, rows)
                for j in range(0, cols))
    return count


def scenic_distance_dir(grid, x, y, dx, dy):
    height = grid[x][y]
    i = x + dx
    j = y + dy
    count = 0
    while 0 <= i < len(grid) and 0 <= j < len(grid[0]):
        count = count + 1
        if height <= grid[i][j]:
            return count
        i += dx
        j += dy
    return count


def scenic_score(grid, x, y):
    return (scenic_distance_dir(grid, x, y, 0, 1)
            * scenic_distance_dir(grid, x, y, 0, -1)
            * scenic_distance_dir(grid, x, y, 1, 0)
            * scenic_distance_dir(grid, x, y, -1, 0))


def max_scenic_score(grid):
    return max(scenic_score(grid, x, y) for x in range(len(grid)) for y in range(len(grid[0])))


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "08.txt") as f:
        grid = input_to_grid(f.readlines(), int)
        print_day(8,
                  count_visible_trees(grid),
                  max_scenic_score(grid))


if __name__ == '__main__':
    main()
