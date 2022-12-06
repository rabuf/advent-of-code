import re
import sys
from pathlib import Path

from aoc_util import print_day


def parse_move(move: str) -> (int, int, int):
    r = re.compile(r'\d+')
    return tuple(map(int, r.findall(move)))


def parse_crates(crates):
    crates = map(list, map(reversed, zip(*crates)))
    return {
        int(row[0]): list(filter(lambda c: c.isalpha(), row))
        for row in crates if row[0].isdigit()
    }


def parse(spec):
    crates, moves = spec.split("\n\n")
    crates = parse_crates(crates.split('\n'))
    moves = [parse_move(move) for move in moves.strip().split('\n')]
    return crates, moves


def crate_hash(crates):
    return ''.join(crates[i][-1] for i in sorted(iter(crates)))


def apply_moves(crates, moves):
    for cnt, src, dst in moves:
        for _ in range(cnt):
            crates[dst].append(crates[src].pop())


def apply_moves_9001(crates, moves):
    for cnt, src, dst in moves:
        crates[dst].extend(crates[src][-cnt:])
        crates[src] = crates[src][:-cnt]


def main():
    input_path = Path(sys.argv[1])
    with open(input_path / "05.txt", "r") as f:
        text = f.read()
        crates, moves = parse(text)
        part1 = {}
        part2 = {}
        for k, v in crates.items():
            part1[k] = v.copy()
            part2[k] = v.copy()
        apply_moves(part1, moves)
        apply_moves_9001(part2, moves)
        print_day(5, crate_hash(part1), crate_hash(part2))


if __name__ == "__main__":
    main()
