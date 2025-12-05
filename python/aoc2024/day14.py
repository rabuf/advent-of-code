import re
import sys
from functools import partial
from pathlib import Path

from aoc_util import print_day


def future(px, py, vx, vy, time=100):
    x = (px + vx * time) % 101
    y = (py + vy * time) % 103
    return x, y


def safety_factor(positions):
    q1, q2, q3, q4 = 0, 0, 0, 0
    for x, y in positions:
        qx, qy = x - 50, y - 51
        if qx < 0 < qy:
            q1 = q1 + 1
        if qx > 0 and qy > 0:
            q2 = q2 + 1
        if qx > 0 > qy:
            q3 = q3 + 1
        if qx < 0 and qy < 0:
            q4 = q4 + 1
    return q1 * q2 * q3 * q4


def parse_line(line):
    return tuple(map(int, re.findall(r"[+-]?\d+", line)))


def print_positions(positions: list):
    for y in range(103):
        for x in range(101):
            p = x, y
            if p in positions:
                print(positions.count(p), end="")
            else:
                print(".", end="")
        print()


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2024" / "14.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
            positions = list(map(future, *zip(*lines)))
            p1 = safety_factor(positions)
            time = 0
            while True:
                positions = list(map(partial(future, time=time), *zip(*lines)))
                if len(set(positions)) == len(positions):
                    break
                time = time + 1
            print_day(14, p1, time)
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
