import sys
from functools import cache
from pathlib import Path

from aoc_util import print_day


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "19.txt") as f:
            patterns, designs = f.read().split('\n\n')
            patterns = set(patterns.split(', '))
            designs = designs.splitlines()

        @cache
        def possible(design):
            if design in patterns:
                return True
            for i in range(1, len(design)):
                l, r = design[:i], design[i:]
                if l in patterns:
                    if possible(r):
                        return True
            return False

        @cache
        def ways(design):
            result = 0
            if design in patterns:
                result = 1
            for i in range(1, len(design)):
                l, r = design[:i], design[i:]
                if l in patterns:
                    result = result + ways(r)
            return result

        p1 = sum(map(possible, designs))
        p2 = sum(map(ways, designs))
        print_day("19", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
