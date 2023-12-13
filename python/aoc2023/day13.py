import sys
from pathlib import Path

from more_itertools import transpose

from aoc_util import print_day


def vertical_reflection(field):
    field = list(transpose(field))
    return horizontal_reflection(field)


def horizontal_reflection(field):
    reflections = []
    for i in range(1, len(field)):
        flipped = list(reversed(field[:i]))
        remaining = field[i:]
        if all(a == b for a, b in zip(remaining, flipped)):
            reflections.append(i)
    return reflections


def find_reflections(field):
    return vertical_reflection(field), horizontal_reflection(field)


def smudge(field: list[str], vertical, horizontal):
    for i in range(len(field)):
        for j in range(len(field[0])):
            new_field = field[:]
            new_field[i] = field[i][:j] + ('.' if field[i][j] == '#' else '.') + field[i][j+1:]
            v, h = find_reflections(new_field)
            v = list(set(v) - set(vertical))
            h = list(set(h) - set(horizontal))
            if not v and not h:
                continue
            if vertical == v and not h:
                continue
            if horizontal == h and not v:
                continue
            return v, h


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "13.txt") as f:
        fields = [field.splitlines() for field in f.read().split('\n\n')]
        reflection_points = [find_reflections(field) for field in fields]
        horizontal = sum(h[0] for _, h in reflection_points if h)
        vertical = sum(v[0] for v, _ in reflection_points if v)
        smudged_reflections = [smudge(field, *reflection) for field, reflection in zip(fields, reflection_points)]
        smudged_horizontal = sum(h[0] for _, h in smudged_reflections if h)
        smudged_vertical = sum(v[0] for v, _ in smudged_reflections if v)
        print_day(13, vertical + 100 * horizontal, smudged_vertical + 100 * smudged_horizontal)


if __name__ == '__main__':
    main()
