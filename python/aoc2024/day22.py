import sys
from collections import defaultdict
from itertools import chain
from pathlib import Path

from more_itertools import windowed

from aoc_util import print_day


def parse_line(line):
    return int(line)


def mix(n, secret):
    return n ^ secret


def prune(secret):
    return secret % 16777216


def process(secret):
    a = secret * 64
    b = mix(a, secret)
    c = prune(b)
    d = c // 32
    e = mix(c, d)
    f = prune(e)
    g = f * 2048
    h = mix(f, g)
    return prune(h)


def part1(secret):
    for _ in range(2000):
        secret = process(secret)
    return secret


def prices(secret):
    yield secret % 10
    for _ in range(2000):
        secret = process(secret)
        yield secret % 10


def sequence_prices(p):
    seen = set()
    for a, b, c, d, e in windowed(p, 5):
        diffs = (b - a, c - b, d - c, e - d)
        if diffs not in seen:
            seen.add(diffs)
            yield diffs, e


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "22.txt") as f:
            secrets = list(map(parse_line, f))
        p1 = sum(map(part1, secrets))
        all_prices = (prices(secret) for secret in secrets)
        deals = (sequence_prices(p) for p in all_prices)
        totals = defaultdict(int)
        for sequence, price in chain.from_iterable(deals):
            totals[sequence] += price
        max_bananas = max(totals.values())
        print_day("22", p1, max_bananas)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
