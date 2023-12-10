import sys
from collections import defaultdict
from pathlib import Path

from aoc_util import print_day


def input_to_grid(lines):
    grid = defaultdict(lambda: '.')
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
    if grid[start + -1j] in '7F|':
        start_direction = -1j
    elif grid[start + 1j] in 'LJ|':
        start_direction = 1j
    elif grid[start + 1] in '-J7':
        start_direction = 1
    else:
        start_direction = -1
    pos = start + start_direction
    direction = start_direction
    loop_only = defaultdict(str)
    loop_only[start] = 'S'
    turns = 0
    while grid[pos] != 'S':
        turns += turning[(grid[pos], direction)]
        direction = next_direction[(grid[pos], direction)]
        loop_only[pos] = grid[pos]
        pos = pos + direction
    print(f'{turns=}')
    frontier = set()
    not_loop = set(grid) - set(loop_only)
    print(f'{(4-1j) in grid.keys()}')
    print(f'{not_loop=}')
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

    print(f'{not_loop=}\n{len(not_loop)=}')
    print(f'{turns=}')
    for pos in list(not_loop):
        if pos.real == 0 or pos.imag == 0:
            frontier.add(pos)
            not_loop.remove(pos)
    print(f'{not_loop}\n{frontier}')
    print(f'{not_loop & frontier=}')
    while frontier:
        pos = frontier.pop()
        for i in [-1, 1, -1j, +1j]:
            potential = pos + i
            if potential in not_loop and potential:
                not_loop.remove(potential)
                frontier.add(potential)
    print(not_loop)
    return len(not_loop)


def loop_length(grid, start):
    if grid[start + -1j] in '7F|':
        direction = -1j
    elif grid[start + 1j] in 'LJ|':
        direction = 1j
    elif grid[start + 1] in '-J7':
        direction = 1
    else:
        direction = -1
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
