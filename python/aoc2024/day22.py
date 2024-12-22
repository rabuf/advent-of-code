import sys
from collections import defaultdict
from pathlib import Path

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
    p = [secret % 10]
    for _ in range(2000):
        secret = process(secret)
        p.append(secret % 10)
    return p


def sequences(p):
    s = [b - a for a, b in zip(p, p[1:])]
    return s


def signals(ss):
    for s in ss:
        for a in zip(s, s[1:], s[2:], s[3:]):
            yield list(a)


def first_sequence_prices(p):
    deal = defaultdict(int)
    for a, b, c, d, e in zip(p, p[1:], p[2:], p[3:], p[4:]):
        diffs = (b - a, c - b, d - c, e - d)
        if diffs not in deal:
            deal[diffs] = e
    return deal


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "22.txt") as f:
            secrets = list(map(parse_line, f.read().splitlines()))
        p1 = sum(map(part1, secrets))
        all_prices = (prices(secret) for secret in secrets)
        deals = [first_sequence_prices(p) for p in all_prices]
        max_bananas = 0
        all_sequences = set()
        for d in deals:
            all_sequences.update(d)
        for seq in all_sequences:
            max_bananas = max(max_bananas, sum(d[seq] for d in deals))
        print_day("22", p1, max_bananas)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
