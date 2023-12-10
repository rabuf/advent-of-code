import sys
from pathlib import Path

from aoc_util import print_day


def input_to_grid(lines):
    grid = {}
    start = 0
    for (row, pipes) in enumerate(lines):
        for (col, pipe) in enumerate(pipes):
            grid[col + row * 1j] = pipe
            if pipe == 'S':
                start = col + row * 1j
    return grid, start


next_direction = {
    ('J', 1): -1j,
    ('J', 1j): -1,
    ('|', 1j): 1j,
    ('|', -1j): -1j,
    ('-', 1): 1,
    ('-', -1): -1,
    ('L', -1): -1j,
    ('L', 1j): 1,
    ('7', 1): 1j,
    ('7', -1j): -1,
    ('F', -1): 1j,
    ('F', -1j): 1,
}

turning = {
    ('J', 1): -1,
    ('J', 1j): 1,
    ('|', 1j): 0,
    ('|', -1j): 0,
    ('-', 1): 0,
    ('-', -1): 0,
    ('L', -1): 1,
    ('L', 1j): -1,
    ('7', 1): 1,
    ('7', -1j): -1,
    ('F', -1): -1,
    ('F', -1j): 1,
}


def enclosed_area(grid, start):
    start_direction = initial_direction(grid, start)
    pos = start + start_direction
    direction = start_direction
    loop_only = {start: 'S'}
    turns = 0
    while grid[pos] != 'S':
        turns += turning[(grid[pos], direction)]
        direction = next_direction[(grid[pos], direction)]
        loop_only[pos] = grid[pos]
        pos = pos + direction
    left = set()
    right = set()
    not_loop = set(grid) - set(loop_only)

    def check(pos, side):
        if pos in not_loop:
            side.add(pos)
            not_loop.remove(pos)

    pos = start + start_direction
    direction = start_direction
    while grid[pos] != 'S':
        match grid[pos], direction:
            case ('|', -1j):
                check(pos + 1, right)
                check(pos - 1, left)
            case ('|', 1j):
                check(pos - 1, right)
                check(pos + 1, left)
            case ('-', 1):
                check(pos + 1j, right)
                check(pos - 1j, left)
            case ('-', -1):
                check(pos - 1j, right)
                check(pos + 1j, left)
            case ('J', 1):
                check(pos + 1j, right)
                check(pos + 1, right)
            case ('L', 1j):
                check(pos - 1, right)
                check(pos + 1j, right)
            case ('7', -1j):
                check(pos + 1, right)
                check(pos - 1j, right)
            case ('F', -1):
                check(pos - 1, right)
                check(pos - 1j, right)
            case ('J', 1j):
                check(pos + 1j, left)
                check(pos + 1, left)
            case ('L', -1):
                check(pos - 1, left)
                check(pos + 1j, left)
            case ('7', 1):
                check(pos + 1, left)
                check(pos - 1j, left)
            case ('F', -1j):
                check(pos - 1, left)
                check(pos - 1j, left)
        direction = next_direction[(grid[pos], direction)]
        pos = pos + direction
    frontier = left if turns < 0 else right
    inside = frontier.copy()
    while frontier:
        pos = frontier.pop()
        for i in [-1, 1, -1j, +1j]:
            potential = pos + i
            if potential in not_loop:
                not_loop.remove(potential)
                frontier.add(potential)
                inside.add(potential)
    return len(inside)


def initial_direction(grid, start):
    if grid.get(start - 1j, '.') in '7F|':
        start_direction = -1j
    elif grid.get(start + 1j, '.') in 'LJ|':
        start_direction = 1j
    elif grid.get(start + 1, '.') in '-J7':
        start_direction = 1
    else:
        start_direction = -1
    return start_direction


def loop_length(grid, start):
    direction = initial_direction(grid, start)
    length = 1
    pos = start + direction
    while pos != start:
        length += 1
        direction = next_direction[(grid[pos], direction)]
        pos = pos + direction
    return length


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "10.txt") as f:
        grid, start = input_to_grid(f.read().splitlines())
        length = loop_length(grid, start)
        enclosed = enclosed_area(grid, start)
        print_day(10, length // 2, enclosed)


if __name__ == '__main__':
    main()
