import re
import sys
from collections import defaultdict
from functools import reduce
from operator import mul
from pathlib import Path

from aoc_util import print_day


def parse_game(game):
    game_id = int(re.search(r"\d+", game).group(0))
    rounds = game.split(":")[1].split(";")
    return game_id, [round_to_dict(round) for round in rounds]


def round_to_dict(round):
    draw = defaultdict(int)
    pairs = re.finditer(r"(\d+) (\w+)", round)
    for p in pairs:
        count, color = p.groups(0)
        draw[color] = int(count)
    return draw


def game_max(rounds):
    game = defaultdict(int)
    for round in rounds:
        for color, count in round.items():
            game[color] = max(game[color], count)
    return game


def possible(game):
    return game["red"] <= 12 and game["green"] <= 13 and game["blue"] <= 14


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "02.txt") as f:
        games = list(map(parse_game, f.read().splitlines()))
        p1 = sum(game_id * possible(game_max(rounds)) for (game_id, rounds) in games)
        p2 = sum(reduce(mul, game_max(rounds).values()) for (game_id, rounds) in games)
        print_day(2, p1, p2)


if __name__ == "__main__":
    main()
