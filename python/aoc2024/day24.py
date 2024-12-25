import random
import sys
from collections import Counter
from itertools import combinations
from pathlib import Path

import z3

from aoc_util import print_day


def parse_initial(line):
    name, value = line.split(': ')
    return name, int(value)


def parse_wire(wire):
    lhs, rhs = wire.split(' -> ')
    l, op, r = lhs.split(' ')
    return rhs, (op, l, r)


def dependencies(values):

    inputs = set(k for k in values if k[0] in 'xy')

    def recur(name, depth=None):
        if depth is not None and depth == 0:
            return []
        if type(values[name]) is int:
            return [name]
        op, l, r = values[name]
        return [name] + recur(l, depth - 1 if depth else None) + recur(r, depth - 1 if depth else None)

    def probables(wires):
        x, y, z = [part1(values, c) for c in 'xyz']
        xy = x + y
        wrong = set()
        for i in range(46):
            if (xy % 2) != z % 2:
                wrong.add(f'z{i:02}')
            xy //= 2
            z //= 2
        return wrong

    initial = probables(values)
    print(sorted(initial))
    for xy in inputs:
        values[xy] = 1

    print(sorted(probables(values)))
    z15 = set(recur('z15')).difference(inputs)
    z30 = set(recur('z30')).difference(inputs)
    for xy in inputs:
        values[xy] = 0 if xy[0] == 'x' else 1
    print(sorted(probables(values)))
    for xy in inputs:
        values[xy] = 0 if xy[0] == 'y' else 1
    print(sorted(probables(values)))
    for xy in inputs:
        values[xy] = 0
    print(sorted(probables(values)))
    for i in range(len(inputs) // 2 + 1):
        values[f'x{i:02}'] = 1
        values[f'y{i:02}'] = 1
        print(f'{i}: {sorted(probables(values))}')
        values[f'x{i:02}'] = 0
        values[f'y{i:02}'] = 0

    print(recur('z05', 2))
    print(recur('rnk', 2))
    good = {'jjj', 'jfb', 'fkn', 'pss', 'cpp', 'rtc'}
    print(len(values) - len(good) - len(inputs))
    for n in recur('z03', None):
        if n in inputs: continue
        if n in good: continue
        _, a, b = values[n]
        print(f'{n}: {values[n]}')


def get_dependencies(values, name):
    if type(values[name]) is int:
        return {name}
    op, l, r = values[name]
    return {name} | get_dependencies(values, l) | get_dependencies(values, r)

def first_wrong(values):
    inputs = set(k for k in values if k[0] in 'xy')

    y = 0
    for i in range(len(inputs) // 2):
        x = 2 ** i
        z = with_x_y(values, x_initial=x)
        if z != x:
            return i
        z = with_x_y(values, y_initial=x)
        if z != x:
            return i
        z = with_x_y(values, x_initial=x, y_initial=x)
        if z & x != 0:
            return i
        if z & (x + x) == 0:
            return i+1
    print('all good')
    return None

def many_trials(values):
    for _ in range(100):
        x = random.randint(0, 2**45-1)
        y = random.randint(0, 2 ** 45 - 1)
        if with_x_y(values, x_initial=x, y_initial=y) != x + y:
            return False
    return True

def part2(values):
    inputs = set(k for k in values if k[0] in 'xy')
    x = y = 0
    for v in reversed(sorted(values)):
        if v[0] == 'x':
            x = x * 2 + values[v]
        if v[0] == 'y':
            y = y * 2 + values[v]
    # print(x, y)
    seen = set()
    def recur(swapped, good):
        w = first_wrong(values)
        if w is None:
            print('huh')
            return swapped
        if len(swapped) == 4:
            print(f'DEADEND: {w}: {sorted(swapped)}')
            return None
        for i in range(w):
            z = f'z{i:02}'
            good = good | get_dependencies(values, z)
        z = f'z{w:02}'
        potentials = get_dependencies(values, z)
        potentials.difference_update(good)
        others = set(values).difference(good)
        # print(f'{w}: {sorted(swapped)}')
        for a in potentials:
            for b in others:
                if a == b:
                    continue
                new = tuple(sorted((a, b)))
                if tuple(sorted(swapped | {new})) in seen:
                    continue
                seen.add(tuple(sorted(swapped | {new})))
                values[a], values[b] = values[b], values[a]
                try:
                    match first_wrong(values):
                        case None:
                            print(z3_validate(values))
                            if many_trials(values):
                                print(with_x_y(values, x_initial=x, y_initial=y), x, y, x + y, with_x_y(values, x_initial=x, y_initial=y) == x + y)
                                names = {s for s, _ in swapped} | {s for _, s in swapped } | {a, b}
                                print(swapped | {(a, b)})
                                return ','.join(sorted(names))
                        case sw if sw > w:
                            if (r := recur(swapped | {new}, good | {a, b})) is not None:
                                return r

                except RecursionError as e:
                    pass
                values[a], values[b] = values[b], values[a]

    return recur(set(), inputs)


def with_x_y(values, prefix='z', x_initial=0, y_initial=0):
    values = values.copy()
    for x in sorted(k for k in values if k[0] == 'x'):
        values[x] = x_initial % 2
        x_initial //= 2
    for y in sorted(k for k in values if k[0] == 'y'):
        values[y] = y_initial % 2
        y_initial //= 2

    def get_value(name):
        if type(values[name]) is int:
            return values[name]
        match values[name]:
            case 'AND', l, r:
                return get_value(l) and get_value(r)
            case 'OR', l, r:
                return get_value(l) or get_value(r)
            case 'XOR', l, r:
                return get_value(l) ^ get_value(r)

    result = 0
    for z in sorted([k for k in values if k[0] == prefix], reverse=True):
        result = result * 2 + get_value(z)
    return result


def part1(values, prefix='z'):
    def get_value(name):
        if type(values[name]) is int:
            return values[name]
        match values[name]:
            case 'AND', l, r:
                return get_value(l) and get_value(r)
            case 'OR', l, r:
                return get_value(l) or get_value(r)
            case 'XOR', l, r:
                return get_value(l) ^ get_value(r)

    result = 0
    for z in sorted([k for k in values if k[0] == prefix], reverse=True):
        result = result * 2 + get_value(z)
    return result


def z3_validate(values):
    variables = {}
    s = z3.Solver()
    xs = [k for k in values if k[0] == 'x']
    ys = [k for k in values if k[0] == 'y']
    zs = [k for k in values if k[0] == 'z']
    x = z3.BitVec('x', len(xs))
    y = z3.BitVec('y', len(ys))
    z = z3.BitVec('z', len(zs))
    s.add(z3.BV2Int(x) + z3.BV2Int(y) == z3.BV2Int(z))
    for xv in xs:
        variables[xv] = z3.BitVec(xv, 1)
        n = int(xv[1:])
        variables[xv] = z3.Extract(n, n, x)
    for yv in ys:
        variables[yv] = z3.BitVec(yv, 1)
        n = int(yv[1:])
        variables[yv] = z3.Extract(n, n, y)
    for zv in zs:
        variables[zv] = z3.BitVec(zv, 1)
        n = int(zv[1:])
        s.add(variables[zv] != z3.Extract(n, n, z))
    for k in values:
        if k in variables:
            continue
        variables[k] = z3.BitVec(k, 1)
    for k, v in values.items():
        if k in xs or k in ys:
            continue
        op, l, r = v
        match op:
            case 'AND':
                variables[k] = variables[l] & variables[r]
            case 'OR':
                variables[k] = variables[l] | variables[r]
            case 'XOR':
                variables[k] = variables[l] ^ variables[r]
    if s.check() == z3.sat:
        print(s.model())
        return True
    return False

def print_chart(values):
    def recur(name, depth=0):
        print(f'{"  " * depth}--{name}')
        if type(values[name]) is int:
            return
        op, l, r = values[name]
        print(f'{"  " * depth}---{op}')
        recur(l, depth + 1)
        recur(r, depth + 1)
    for z in sorted(k for k in values if k[0] == 'z')[:10]:
        recur(z)

def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "24.txt") as f:
            initial, wires = f.read().split('\n\n')
        initial = initial.splitlines()
        wires = wires.splitlines()
        initial = list(map(parse_initial, initial))
        wires = list(map(parse_wire, wires))
        values = dict(initial)
        values.update(wires)
        p1 = part1(values)
        p2 = part2(values) # dependencies(values)
        print_day("24", p1, p2, len(wires))
        assert p2 == 'gbf,hdt,jgt,mht,nbf,z05,z09,z30'
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
