import sys
from collections import defaultdict
from functools import cmp_to_key, partial
from pathlib import Path

from more_itertools.more import is_sorted
from more_itertools.recipes import partition

from aoc_util import print_day


def parse_rules(rules: str):
    result = defaultdict(set)
    for rule in rules.splitlines():
        l, r = list(map(int, rule.split('|')))
        result[l].add(r)
    return result


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "05.txt") as f:
        rules, printouts = f.read().split('\n\n')
        rules = parse_rules(rules)

        def key(a, b):
            if a == b:
                return 0
            if a in rules and b in rules[a]:
                return -1
            return 1

        printouts = [list(map(int, printout.split(','))) for printout in printouts.splitlines()]

        valid = partial(is_sorted, key=cmp_to_key(key))
        fix = partial(sorted, key=cmp_to_key(key))
        invalids, valids = partition(valid, printouts)
        p1 = sum(p[len(p) // 2] for p in valids)
        p2 = sum(p[len(p) // 2] for p in map(fix, invalids))

        print_day(5, p1, p2)


if __name__ == '__main__':
    main()
