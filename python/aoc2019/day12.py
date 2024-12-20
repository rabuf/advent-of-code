import math
import re
import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return list(map(int, re.findall(r'[+-]?\d+', line)))


def cycle_detection(f, x0):
    power = lam = 1
    tortoise = x0
    hare = f(x0)
    while tortoise != hare:
        if power == lam:
            tortoise = hare
            power *= 2
            lam = 0
        hare = f(hare)
        lam += 1

    tortoise = hare = x0
    for i in range(lam):
        hare = f(hare)

    mu = 0
    while tortoise != hare:
        tortoise = f(tortoise)
        hare = f(hare)
        mu += 1
    return lam, mu


def cycle_length(nums):
    return cycle_detection(step, (nums, [0] * len(nums)))


def step(nums):
    pos, vel = nums
    gravity = calculate_gravity(pos)
    vel = [v + g for v, g in zip(vel, gravity)]
    pos = [p + v for p, v in zip(pos, vel)]
    return pos, vel


def calculate_gravity(nums):
    gravity = []
    for i in range(len(nums)):
        gravity.append(sum(1 if nums[i] < nums[j] else -1 if nums[i] > nums[j] else 0 for j in range(len(nums))))
    return gravity


def energy(nums):
    pos, vel = nums
    return sum(map(abs, pos)) * sum(map(abs, vel))


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "12.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        planets = xs, ys, zs = [[p[i] for p in lines] for i in range(3)]
        cycles = list(map(cycle_length, planets))
        p2 = math.lcm(*(l for l, _ in cycles))
        xs = xs, [0] * len(xs)
        ys = ys, [0] * len(ys)
        zs = zs, [0] * len(zs)
        for _ in range(1000):
            xs = step(xs)
            ys = step(ys)
            zs = step(zs)
        pxs, vxs = xs
        pys, vys = ys
        pzs, vzs = zs
        pos = zip(pxs, pys, pzs)
        vel = zip(vxs, vys, vzs)
        p1 = sum(map(energy, zip(pos, vel)))
        print_day("12", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
