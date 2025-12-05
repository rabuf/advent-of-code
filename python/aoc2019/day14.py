import sys
from collections import defaultdict
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    inputs, output = line.split("=>")
    inputs = inputs.split(",")
    inputs = [i.strip().split(" ") for i in inputs]
    inputs = [(int(left), right) for left, right in inputs]
    left, right = output.strip().split(" ")
    output = int(left), right
    return inputs, output


def required_ore(rules):
    available = defaultdict(int)
    consumed = defaultdict(int)

    def recur(n, item):
        if item == "ORE":
            consumed["ORE"] += n
        elif available[item] >= n:
            available[item] -= n
            consumed[item] += n
        else:
            c, items = rules[item]
            for m, i in items:
                recur(m, i)
            available[item] += c
            recur(n, item)

    recur(1, "FUEL")
    return consumed["ORE"] + available["ORE"]


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2019" / "14.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        rules = {}
        for i, (n, o) in lines:
            rules[o] = (n, i)
        p1 = required_ore(rules)
        p2 = p1

        print_day("14", p1, p2, lines, len(lines))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
