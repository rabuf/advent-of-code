import sys
from functools import reduce
from operator import mul, or_
from pathlib import Path

from aoc_util import print_day, input_to_grid


def scenic_distance(grid, x, y, dx, dy):
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


def sees_edge(grid, x, y, dx, dy):
    height = grid[x][y]
    i = x + dx
    j = y + dy
    while 0 <= i < len(grid) and 0 <= j < len(grid[0]):
        if height <= grid[i][j]:
            return False
        i += dx
        j += dy
    return True


def print_grid(grid):
    for row in grid:
        for c in row:
            print(c, end='')
        print()


def search_from(grid, x, y, op=None, func=None):
    return reduce(op, [func(grid, x, y, 0, 1),
                       func(grid, x, y, 0, -1),
                       func(grid, x, y, 1, 0),
                       func(grid, x, y, -1, 0)])


def search(grid, op=None, func=None, res=None):
    return res(search_from(grid, x, y, op=op, func=func)
               for x in range(len(grid))
               for y in range(len(grid[0])))


def count_visible_trees(grid):
    return search(grid, op=or_, func=sees_edge, res=sum)


def max_scenic_score(grid):
    return search(grid, op=mul, func=scenic_distance, res=max)


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "08.txt") as f:
        grid = input_to_grid(f.readlines(), int)
        print_day(8,
                  count_visible_trees(grid),
                  max_scenic_score(grid))


if __name__ == '__main__':
    main()
