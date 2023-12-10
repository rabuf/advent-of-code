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
    loop_only = {}
    loop_only[start] = 'S'
    turns = 0
    while grid[pos] != 'S':
        turns += turning[(grid[pos], direction)]
        direction = next_direction[(grid[pos], direction)]
        loop_only[pos] = grid[pos]
        pos = pos + direction
    frontier = set()
    not_loop = set(grid) - set(loop_only)

    def check(pos):
        if pos in not_loop:
            frontier.add(pos)
            not_loop.remove(pos)

    pos = start + start_direction
    direction = start_direction
    while grid[pos] != 'S':
        if turns < 0:
            match grid[pos], direction:
                case ('|', -1j):
                    check(pos + 1)
                case ('|', 1j):
                    check(pos - 1)
                case ('-', 1):
                    check(pos + 1j)
                case ('-', -1):
                    check(pos - 1j)
                case ('J', 1):
                    check(pos + 1j)
                    check(pos + 1)
                case ('L', 1j):
                    check(pos - 1)
                    check(pos + 1j)
                case ('7', -1j):
                    check(pos + 1)
                    check(pos - 1j)
                case ('F', -1):
                    check(pos - 1)
                    check(pos - 1j)
        else:
            match grid[pos], direction:
                case ('|', -1j):
                    check(pos - 1)
                case ('|', 1j):
                    check(pos + 1)
                case ('-', 1):
                    check(pos - 1j)
                case ('-', -1):
                    check(pos + 1j)
                case ('J', 1j):
                    check(pos + 1j)
                    check(pos + 1)
                case ('L', -1):
                    check(pos - 1)
                    check(pos + 1j)
                case ('7', 1):
                    check(pos + 1)
                    check(pos - 1j)
                case ('F', -1j):
                    check(pos - 1)
                    check(pos - 1j)
        direction = next_direction[(grid[pos], direction)]
        pos = pos + direction
    for pos in list(not_loop):
        if pos.real == 0 or pos.imag == 0:
            frontier.add(pos)
            not_loop.remove(pos)
    while frontier:
        pos = frontier.pop()
        for i in [-1, 1, -1j, +1j]:
            potential = pos + i
            if potential in not_loop and potential:
                not_loop.remove(potential)
                frontier.add(potential)
    return len(not_loop)


def initial_direction(grid, start):
    if (start - 1j) in grid and grid[start + -1j] in '7F|':
        start_direction = -1j
    elif (start + 1j) in grid and grid[start + 1j] in 'LJ|':
        start_direction = 1j
    elif (start + 1) in grid and grid[start + 1] in '-J7':
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
    with open(input_dir / "2023" / "10.test.4.txt") as f:
        grid, start = input_to_grid(f.read().splitlines())
        length = loop_length(grid, start)
        enclosed = enclosed_area(grid, start)
        print_day(10, length // 2, enclosed)


if __name__ == '__main__':
    main()
