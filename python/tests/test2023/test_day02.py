from hypothesis import given, strategies as st
from pytest import mark

from aoc2023.day02 import *

day2_part1_sample = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

day2_part1_parsed = [
    (1, [{'blue': 3, 'red': 4},
         {'red': 1, 'green': 2, 'blue': 6},
         {'green': 2}]),
    (2, [{'blue': 1, 'green': 2},
         {'green': 3, 'blue': 4, 'red': 1},
         {'green': 1, 'blue': 1}]),
    (3, [{'green': 8, 'blue': 6, 'red': 20},
         {'blue': 5, 'red': 4, 'green': 13},
         {'green': 5, 'red': 1}]),
    (4, [{'green': 1, 'red': 3, 'blue': 6},
         {'green': 3, 'red': 6},
         {'green': 3, 'blue': 15, 'red': 14}]),
    (5, [{'red': 6, 'blue': 1, 'green': 3},
         {'blue': 2, 'red': 1, 'green': 2}])
]

day2_part1_expected = 8  # 1, 2, 5 possible, 3 and 4 impossible

day2_part1_possible = [True, True, False, False, True]

day2_part2_values = [48, 12, 1560, 630, 36]


@mark.parametrize("game,expected", zip(day2_part1_sample.splitlines(), day2_part1_parsed))
def test_day2_parsing(game, expected):
    assert parse_game(game) == expected


@given(game_id=st.integers(min_value=0), draws=st.lists(
    st.dictionaries(keys=st.sampled_from(['red', 'green', 'blue']), values=st.integers(min_value=1, max_value=20),
                    min_size=1), min_size=1))
def test_random_parsing(game_id, draws):
    """
    This is just fooling around. It'll generate a whole bunch of random sets, I construct the input and test
    that my parsing function produces the same result. It generates an int for the id and a list[dict] for
    the rounds. Each round is guaranteed to have at least one cube color and each game is guaranteed at
    least one round.
    """
    game = f'Game {game_id}: {"; ".join(", ".join(f"{val} {color}" for color, val in draw.items()) for draw in draws)}'
    parsed_id, parsed_draws = parse_game(game)
    assert parsed_id == game_id
    assert parsed_draws == draws


def test_day2_part1():
    games = map(parse_game, day2_part1_sample.splitlines())
    p1 = sum(game_id * possible(game_max(rounds)) for (game_id, rounds) in games)
    assert p1 == 8


def test_day2_part2():
    games = map(parse_game, day2_part1_sample.splitlines())
    p2 = sum(reduce(mul, game_max(rounds).values()) for (game_id, rounds) in games)
    assert p2 == sum(day2_part2_values)


@mark.parametrize("game,expected", zip(day2_part1_sample.splitlines(), day2_part1_possible))
def test_day2_part1_possible(game, expected):
    (game_id, rounds) = parse_game(game)
    maxes = game_max(rounds)
    assert possible(maxes) == expected


@mark.parametrize("game,expected", zip(day2_part1_sample.splitlines(), day2_part2_values))
def test_day2_part2_parts(game, expected):
    (game_id, rounds) = parse_game(game)
    maxes = game_max(rounds)
    assert reduce(mul, maxes.values()) == expected
