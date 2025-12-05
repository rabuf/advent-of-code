import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    direction, length, color = line.split()
    direction = {"R": 1, "L": -1, "U": -1j, "D": 1j}[direction]
    length = int(length)
    return direction, length, color


def coordinates(lines):
    coord = [0]
    for direction, length in lines:
        coord.append(coord[-1] + direction * length)
    return coord


def shoelace(coords):
    return (
        int(
            sum(
                (i.real * j.imag - i.imag * j.real + abs(i - j))
                for i, j in zip(coords, coords[1:] + coords[:1])
            )
        )
        // 2
        + 1
    )


def decode(color):
    color = color.strip("()")
    direction = {"0": 1, "1": 1j, "2": -1, "3": -1j}[color[-1]]
    length = int(color[1:-1], 16)
    return direction, length


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "18.txt") as f:
        lines = map(parse_line, f.read().splitlines())
    directions, lengths, colors = list(zip(*lines))
    path = zip(directions, lengths)
    coords = coordinates(path)
    area = shoelace(coords)
    decoded_path = map(decode, colors)
    decoded_coords = coordinates(decoded_path)
    decoded_area = shoelace(decoded_coords)
    print_day(18, area, decoded_area)


if __name__ == "__main__":
    main()
