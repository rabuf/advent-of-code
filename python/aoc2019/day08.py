import sys
from collections import Counter
from pathlib import Path

from more_itertools.recipes import batched

from aoc_util import print_day


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2019" / "08.txt") as f:
            lines = f.read().strip()
        minimum = len(lines)
        p1 = 0
        for line in batched(lines, 25 * 6):
            counts = Counter(line)
            if counts["0"] < minimum:
                minimum = counts["0"]
                p1 = counts["1"] * counts["2"]
        image = ["2"] * 25 * 6
        for line in batched(lines, 25 * 6):
            for i, p in enumerate(line):
                if image[i] == "2":
                    image[i] = p
        for row in batched(image, 25):
            for c in row:
                print("#" if c == "1" else " ", end="")
            print()
        print_day("08", p1, "EHRUE")
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
