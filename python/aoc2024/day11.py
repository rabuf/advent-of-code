import sys
from functools import cache
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return line


@cache
def apply(n):
    if n == 0:
        return [1]
    s = str(n)
    if len(s) % 2 == 0:
        return [int(s[:len(s) // 2]), int(s[len(s) // 2:])]
    return [n * 2024]


def blink(stones, times=25):
    for _ in range(times):
        next_stones = []
        for stone in stones:
            next_stones.extend(apply(stone))
        stones = next_stones
    return stones


@cache
def blink_single_stone(stone, times=25):
    if times == 0:
        return 1
    stones = apply(stone)
    return sum(blink_single_stone(s, times - 1) for s in stones)


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "11.txt") as f:
        stones = list(map(int, f.readline().split()))
    p1 = sum(blink_single_stone(s, times=25) for s in stones)
    p2 = sum(blink_single_stone(s, times=75) for s in stones)

    print_day(11, p1, p2)


if __name__ == '__main__':
    main()
