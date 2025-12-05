import sys
from pathlib import Path
from queue import PriorityQueue

from aoc_util import print_day


def least_heat(grid, height, width, minimum_streak=0, maximum_steak=3):
    frontier = PriorityQueue()
    frontier.put((grid[(1, 0)], (1, 0), (1, 0), 0))
    frontier.put((grid[(0, 1)], (0, 1), (0, 1), 0))
    visited = set()
    target = (width - 1, height - 1)
    while not frontier.empty():
        cost, (x, y), (dx, dy), streak = frontier.get()
        if (x, y) == target and minimum_streak <= streak:
            return cost
        if ((x, y), (dx, dy), streak) in visited:
            continue
        visited.add(((x, y), (dx, dy), streak))
        if streak < (maximum_steak - 1) and (x + dx, y + dy) in grid:
            straight_position = (x + dx, y + dy)
            straight_cost = cost + grid[straight_position]
            frontier.put((straight_cost, straight_position, (dx, dy), streak + 1))
        if minimum_streak <= streak:
            lx, ly = dy, -dx
            left_position = (x + lx, y + ly)
            rx, ry = -dy, dx
            right_position = (x + rx, y + ry)
            if left_position in grid:
                left_cost = cost + grid[left_position]
                frontier.put((left_cost, left_position, (lx, ly), 0))
            if right_position in grid:
                right_cost = cost + grid[right_position]
                frontier.put((right_cost, right_position, (rx, ry), 0))


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "17.txt") as f:
        lines = f.read().splitlines()
    height, width = len(lines), len(lines[0])
    grid = {(x, y): int(n) for y, line in enumerate(lines) for x, n in enumerate(line)}
    p1 = least_heat(grid, height, width)
    p2 = least_heat(grid, height, width, minimum_streak=3, maximum_steak=10)
    print_day(17, p1, p2)


if __name__ == "__main__":
    main()
