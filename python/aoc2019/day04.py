import re
import sys
from pathlib import Path

from more_itertools.more import is_sorted

from aoc_util import print_day


def parse_line(line):
    return line


def main():
    input_dir = Path(sys.argv[1])
    try:
        r = [231832, 767346]
        p1 = 0
        p2 = 0
        for password in range(r[0], r[1] + 1):
            p = str(password)
            if is_sorted(p) and (m := re.findall(r'(\d)\1', p)):
                p1 = p1 + 1
                if any(re.match(rf'\d*{d * 3}\d*', p) is None for d in m):
                    p2 = p2 + 1
        print_day("04", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
