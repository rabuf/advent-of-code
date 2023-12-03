import sys
from pathlib import Path

from aoc_util import print_day

directions = {
    'U': 1j,
    'D': -1j,
    'R': 1,
    'L': -1,
}


def sign(x):
    return (x > 0) - (x < 0)


def move(snake):
    for i in range(1, len(snake)):
        dist = snake[i - 1] - snake[i]
        if abs(dist) >= 2:
            snake[i] += complex(sign(dist.real), sign(dist.imag))
    return snake


def parse_line(line):
    direction, distance = line.split()
    return directions[direction], int(distance)


def apply_direction(direction, snake):
    snake[0] += direction
    move(snake)


def part_1(lines, length=2):
    snake = [0] * length
    locations = [{x} for x in snake]
    for (direction, dist) in lines:
        for _ in range(dist):
            apply_direction(direction, snake)
            [locations[i].add(snake[i]) for i in range(length)]
    return locations


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2022" / "09.txt") as f:
        lines = list(map(parse_line, f.read().splitlines()))
        visited = part_1(lines, length=10)
        print_day(9, len(visited[1]), len(visited[9]))


if __name__ == '__main__':
    main()
