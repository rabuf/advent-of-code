import sys
from pathlib import Path

import regex

from aoc_util import print_day


def name_to_number(name):
    values = {
        "one": "1",
        "two": "2",
        "three": "3",
        "four": "4",
        "five": "5",
        "six": "6",
        "seven": "7",
        "eight": "8",
        "nine": "9",
    }
    return values[name] if name in values else name


def line_to_calibration_value(line, pattern=r"(\d)"):
    nums = regex.findall(pattern, line, overlapped=True)
    return int(name_to_number(nums[0]) + name_to_number(nums[-1]))


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "01.txt") as f:
        lines = f.read().splitlines()
        p1 = sum(line_to_calibration_value(line) for line in lines)
        p2 = sum(
            line_to_calibration_value(
                line, r"(\d|one|two|three|four|five|six|seven|eight|nine)"
            )
            for line in lines
        )
        print_day(1, p1, p2)


if __name__ == "__main__":
    main()
