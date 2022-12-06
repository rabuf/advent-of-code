import sys
from pathlib import Path

from aoc_util import print_day


def start_of_packet(tx):
    for i in range(4, len(tx)):
        if len(set(tx[i-4:i])) == 4:
            return i


def start_of_message(tx):
    for i in range(14, len(tx)):
        if len(set(tx[i-14:i])) == 14:
            return i


# Better version of the above
def start_of_marker(tx, length=4):
    for i in range(length, len(tx)):
        if len(set(tx[i-length:i])) == length:
            return i


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "06.txt") as f:
        tx = f.read()
        print_day(6, start_of_packet(tx), start_of_message(tx))


if __name__ == '__main__':
    main()