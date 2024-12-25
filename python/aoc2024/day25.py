import sys
from pathlib import Path

from more_itertools.recipes import transpose, partition

from aoc_util import print_day


def key_or_lock(data):
    transposed = list(transpose(data.splitlines()))
    counts = tuple(t.count('#') - 1 for t in transposed)
    if transposed[0][0] == '#':
        return 'lock', counts
    else:
        return 'key', counts


def match(key, lock):
    return all(k + l <= 5 for k, l in zip(key, lock))


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "25.txt") as f:
            lines = f.read().split('\n\n')

        keys_and_locks = list(map(key_or_lock, lines))
        keys, locks = partition(lambda x: x[0] == 'key', keys_and_locks)
        keys = list(k for _, k in keys)
        locks = list(l for _, l in locks)
        result = 0
        for k in keys:
            for l in locks:
                result += match(k, l)
        print_day("25", result)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
