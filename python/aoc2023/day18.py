import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    direction, length, color = line.split()
    direction = {'R': 1, 'L': -1, 'U': -1j, 'D': 1j}[direction]
    length = int(length)
    return direction, length, color


def print_grid(grid, top_left, bottom_right):
    for y in range(int(top_left.imag)-1, int(bottom_right.imag) + 1):
        for x in range(int(top_left.real), int(bottom_right.real) + 1):
            position = x + y * 1j
            if position == 0:
                print('S', end='')
                continue
            if position in grid:
                print('#', end='')
            else:
                print(' ', end='')
        print()


def dig_out(lines):
    grid = {}
    position = 0
    min_x, min_y, max_x, max_y = 0, 0, 0, 0
    for direction, length, color in lines:
        for _ in range(length):
            position += direction
            grid[position] = color
            min_x = min(min_x, position.real)
            max_x = max(max_x, position.real)
            min_y = min(min_y, position.imag)
            max_y = max(max_y, position.imag)
    return grid, min_x + min_y * 1j, max_x + max_y * 1j


def fill_in(grid, top_left, bottom_right):
    frontier = {1 + 1j}
    visited = set()
    while frontier:
        consider = frontier.pop()
        if consider.imag < top_left.imag or consider.imag > bottom_right.imag:
            continue
        if consider.real < top_left.real or consider.real > bottom_right.real:
            continue
        if consider not in visited:
            new = {consider + direction for direction in [1, -1, 1j, -1j]}
            new = new - set(grid)
            frontier = frontier | new
            grid[consider] = '#'


def coordinates(lines):
    coord = [0]
    for direction, length in lines:
        coord.append(coord[-1] + direction * length)
    return coord


def shoelace(coords):
    area = 0
    for left, right in zip(coords, coords[1:] + coords[0:1]):
        area += left.real * right.imag - left.imag * right.real + abs(left.real - right.real) + abs(left.imag - right.imag)
    return int(area + 2) // 2


def decode(color):
    color = color.strip('()')
    direction = {'0': 1, '1': 1j, '2': -1, '3': -1j}[color[-1]]
    length = int(color[1:-1], 16)
    return direction, length


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "18.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
    path = [(direction, length) for direction, length, _ in lines]
    coords = coordinates(path)
    area = shoelace(coords)
    decoded = [decode(color) for _, _, color in lines]
    decoded_coords = coordinates(decoded)
    decoded_area = shoelace(decoded_coords)
    print_day(18, area, decoded_area)


if __name__ == '__main__':
    main()
