import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    return [int(i) for i in line]


def best_joltage(bank, size=2):
    joltage, rest = bank[:size], bank[size:]
    for c in rest:
        best = int("".join(map(str, joltage)))
        for i in range(size):
            contender = int("".join(map(str, joltage[:i] + joltage[i + 1 :] + [c])))
            best = max(best, contender)
        joltage = list(map(int, str(best)))
    return int("".join(map(str, joltage)))


def part1(banks):
    return sum(map(best_joltage, banks))


def part2(banks):
    def best(b):
        return best_joltage(b, 12)

    return sum(map(best, banks))


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2025" / "03.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        print_day("03", part1(lines), part2(lines))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
