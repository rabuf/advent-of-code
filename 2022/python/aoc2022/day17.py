import sys
from itertools import cycle
from pathlib import Path

from more_itertools import nth_or_last

from aoc_util import print_day

pieces = [{3, 4, 5, 6},
          {4, 3 + 1j, 4 + 1j, 5 + 1j, 4 + 2j},
          {3, 4, 5, 5 + 1j, 5 + 2j},
          {3, 3 + 1j, 3 + 2j, 3 + 3j},
          {3, 4, 3 + 1j, 4 + 1j}]


def make_piece(level):
    for piece in cycle(pieces):
        level = yield {part + (level * 1j) for part in piece}


def wind_cycle(winds):
    return cycle(-1 if w == '<' else 1 for w in winds)


def can_drop(piece, world):
    for part in piece:
        if part - 1j in world or part.imag - 1 == 0:
            return False
    return True


def can_shift(piece, direction, world):
    for part in piece:
        if part + direction in world or (part.real + direction) not in range(1, 8):
            return False
    return True


def drop_piece(piece):
    return {part - 1j for part in piece}


def shift_piece(piece, direction):
    return {part + direction for part in piece}


def play_game(wind):
    level = 0
    pieces = make_piece(level + 4)
    winds = wind_cycle(wind)
    current = next(pieces)
    world = set()
    for direction in winds:
        if can_shift(current, direction, world):
            current = shift_piece(current, direction)
        if can_drop(current, world):
            current = drop_piece(current)
        else:
            world.update(current)
            level = max(level, max(int(part.imag) for part in current))
            yield level
            current = pieces.send(level + 4)


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "17.txt") as f:
        winds = f.readline().strip()
    game = play_game(winds)
    level = nth_or_last(game, 2021)
    print_day(17, level)


if __name__ == '__main__':
    main()
