import sys
from functools import cache
from pathlib import Path

from aoc_util import print_day

key_to_pos = {
    'A': 0,
    '0': -1,
    '1': -2 + 1j,
    '2': -1 + 1j,
    '3': 1j,
    '4': -2 + 2j,
    '5': -1 + 2j,
    '6': 2j,
    '7': -2 + 3j,
    '8': -1 + 3j,
    '9': 3j,
}

arrow_to_pos = {
    'A': 0,
    '^': -1,
    '<': -2 - 1j,
    'v': -1 - 1j,
    '>': -1j,
}

dir_to_arrow = {
    0: 'A',
    -1: '<',
    1: '>',
    -1j: 'v',
    1j: '^',
}


@cache
def optimal_direction(start, end):
    distance = end - start
    path = []
    x, y = int(distance.real), int(distance.imag)
    xs = (x > 0) - (x < 0)
    ys = ((0 < y) - (y < 0)) * 1j
    match x, y:
        case 0, 0:
            pass
        case x, 0:
            path.append(dir_to_arrow[xs] * abs(x))
        case 0, y:
            path.append(dir_to_arrow[ys] * abs(y))
        case x, y if start + x == -2:
            path.append(dir_to_arrow[ys] * abs(y) + dir_to_arrow[xs] * abs(x))
        case x, y if start + y * 1j == -2:
            path.append(dir_to_arrow[xs] * abs(x) + dir_to_arrow[ys] * abs(y))
        case x, y if x < 0:
            path.append(dir_to_arrow[xs] * abs(x) + dir_to_arrow[ys] * abs(y))
        case x, y:
            path.append(dir_to_arrow[ys] * abs(y) + dir_to_arrow[xs] * abs(x))
    path.append('A')
    return ''.join(path)


def solve(digits, depth):
    @cache
    def recur(a, b, d):
        steps = optimal_direction(arrow_to_pos[a], arrow_to_pos[b])
        if d == 0:
            return len(steps)
        result = 0
        for m, n in zip('A' + steps, steps):
            result += recur(m, n, d - 1)
        return result

    initial = []
    for a, b in zip('A' + digits, digits):
        initial.extend(optimal_direction(key_to_pos[a], key_to_pos[b]))
    initial = ''.join(initial)
    result = 0
    for a, b in zip('A' + initial, initial):
        result += recur(a, b, depth)
    return result


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "21.txt") as f:
            lines = f.read().splitlines()
        p1 = fast_solver(1, lines)
        p2 = fast_solver(24, lines)
        print_day("21", p1, p2)
    except IOError as e:
        print(e)


def fast_solver(depth, lines):
    p2 = 0
    for digits in lines:
        l = solve(digits, depth)
        n = int(digits[:-1])
        p2 += l * n
    return p2


if __name__ == '__main__':
    main()
