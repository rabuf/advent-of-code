import sys
from itertools import cycle, islice, tee
from pathlib import Path

from more_itertools import nth_or_last, unzip

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


def make_relative(top, levels):
    # top = max(levels)
    return [top - l for l in levels]


def play_game(wind):
    level = 0
    pieces = make_piece(level + 4)
    wind_length = len(wind)
    winds = wind_cycle(wind)
    current = next(pieces)
    world = set()
    wind_index = -1
    rock_index = 0
    levels = [0] * 7
    for direction in winds:
        wind_index = (wind_index + 1) % wind_length
        if can_shift(current, direction, world):
            current = shift_piece(current, direction)
        if can_drop(current, world):
            current = drop_piece(current)
        else:
            world.update(current)
            for part in current:
                i = int(part.real)
                levels[i - 1] = max(levels[i - 1], int(part.imag))
            level = max(levels)
            yield level, (rock_index, wind_index, make_relative(level, levels))
            current = pieces.send(level + 4)
            rock_index = (rock_index + 1) % 5


def floyd(winds):
    _, fingerprints = unzip(play_game(winds))
    t1, t2, h = tee(fingerprints, 3)

    next(t1)
    tortoise = next(t1)
    next(h)
    next(h)
    hare = next(h)
    while tortoise != hare:
        tortoise = next(t1)
        next(h)
        hare = next(h)

    mu = 0
    tortoise = next(t2)
    while tortoise != hare:
        tortoise = next(t2)
        hare = next(h)
        mu += 1

    lam = 1
    hare = next(t2)
    while tortoise != hare:
        hare = next(t2)
        lam += 1

    return lam, mu


def brent(winds):
    '''This will return lambda and mu, the cycle length and the "remainder", initial
    portion preceding the cycle. Using this, the level for 1 trillion can be calculated.'''
    game = play_game(winds)
    _, fingerprints = unzip(game)
    l, f1, f2 = tee(fingerprints, 3)
    power = lam = 1
    tortoise = next(l)
    hare = next(l)
    while tortoise != hare:
        if power == lam:
            tortoise = hare
            power += 2
            lam = 0
        hare = next(l)
        lam += 1

    mu = 0
    tortoise = next(f1)
    hare = next(f2)
    for i in range(lam):
        hare = next(f2)

    while tortoise != hare:
        tortoise = next(f1)
        hare = next(f2)
        mu += 1

    return lam, mu


def high_level(winds, number_pieces):
    lam, mu = floyd(winds)
    extra = (number_pieces - mu) % lam
    game = play_game(winds)
    heights, _ = unzip(game)
    heights = list(islice(heights, mu + lam + extra + 1))
    mu_height = heights[mu - 1]
    lam_height = heights[mu + lam - 1] - mu_height
    extra_height = heights[mu + lam + extra - 1] - lam_height - mu_height

    target_height = mu_height + extra_height + lam_height * ((number_pieces - mu) // lam)

    return target_height


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "17.txt") as f:
        winds = f.readline().strip()
    game = play_game(winds)
    level, _ = nth_or_last(game, 2021)
    print_day(17, level, high_level(winds, 1000000000000))


if __name__ == '__main__':
    main()
