from itertools import islice

from hypothesis import given, strategies as st
from more_itertools import unzip

from aoc2022 import day17


@given(st.integers())
def test_pieces_start_at_specified_level(level):
    pieces = day17.make_piece(level)
    assert next(pieces) == {part + level * 1j for part in day17.pieces[0]}


@given(st.integers())
def test_second_piece_at_specified_level(level):
    pieces = day17.make_piece(0)
    next(pieces)  # discarded
    assert pieces.send(level) == {part + level * 1j for part in day17.pieces[1]}


def test_make_all_pieces():
    pieces = day17.make_piece(0)
    p = [next(pieces)]
    for i in range(1, 5):
        p.append(pieces.send(0))
    for i, piece in enumerate(p):
        assert piece == day17.pieces[i]


def test_cycles_pieces():
    pieces = day17.make_piece(0)
    p = [next(pieces)]
    for i in range(1, 10):
        p.append(pieces.send(0))
    for i, piece in enumerate(p):
        assert piece == day17.pieces[i % 5]


def test_wind_cycle_always_left():
    wind = day17.wind_cycle('<')
    for direction in islice(wind, 5):
        assert direction == -1


def test_wind_cycle_always_right():
    wind = day17.wind_cycle('>')
    for direction in islice(wind, 5):
        assert direction == 1


def test_wind_cycle_alternates():
    wind = day17.wind_cycle('<>')
    for i, direction in enumerate(islice(wind, 5)):
        assert direction == 1 if i % 2 else -1


sample = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"


def test_play_game():
    game = day17.play_game(sample)
    levels, _ = unzip(game)
    assert next(levels) == 1
    assert next(levels) == 4
    assert next(levels) == 6


def test_play_game_2022():
    game = day17.play_game(sample)
    levels, _ = unzip(game)
    assert next(islice(levels, 2021, 2022)) == 3068


def test_find_cycle():
    assert day17.brent(sample) == (35, 27)


def test_high_level():
    assert day17.high_level(sample, 1000000000000) == 1514285714288
