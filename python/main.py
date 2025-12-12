import datetime

from aoc_util import load_and_run


def main():
    for year in range(2025, 2026):
        year_start = datetime.datetime.now()
        days = 0
        for day in range(1, 26):
            days += load_and_run(year, day)
        year_end = datetime.datetime.now()
        if days:
            print(f"[{year}: {year_end - year_start}]")


if __name__ == "__main__":
    main()
