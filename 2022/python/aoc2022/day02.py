import sys
from enum import IntEnum
from pathlib import Path

from aoc_util import print_day


class RPS(IntEnum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3


a_beats_b = {
    RPS.ROCK: RPS.SCISSORS,
    RPS.PAPER: RPS.ROCK,
    RPS.SCISSORS: RPS.PAPER,
}


def beats(first: RPS, second: RPS) -> bool:
    return second == a_beats_b[first]


def decode(c, base='A'):
    return RPS(ord(c) - ord(base) + 1)


def parse_line(line):
    return decode(line[0]), decode(line[2], 'X')


def score(elf, me):
    return me + 3 * (elf == me) + 6 * beats(me, elf)


def parse(lines):
    return [parse_line(line) for line in lines]


def strategy(opp, strat):
    match strat:
        case RPS.ROCK:
            return a_beats_b[opp]
        case RPS.PAPER:
            return opp
        case RPS.SCISSORS:
            return a_beats_b[a_beats_b[opp]]


def solve(plays):
    a = sum(map(score, *zip(*plays)))
    real_plays = [(opp, strategy(opp, play)) for opp, play in plays]
    b = sum(map(score, *zip(*real_plays)))
    return a, b


def main():
    input_path = Path(sys.argv[1])
    with open(input_path / "2022" / "02.txt", "r") as f:
        plays = parse(f)
        print_day(2, *solve(plays))


if __name__ == "__main__":
    main()
