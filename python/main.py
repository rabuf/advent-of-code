import datetime
import importlib


def main():
    for year in range(2025, 2026):
        year_start = datetime.datetime.now()
        days = 0
        for day in range(1, 26):
            days += load_and_run(year, day)
        year_end = datetime.datetime.now()
        if days:
            print(f'[{year}: {year_end - year_start}]')


def load_and_run(year, day):
    try:

        m = importlib.import_module(f'aoc{year}.day{day:02}')
        start = datetime.datetime.now()
        if hasattr(m, 'main'):
            m.main()
        end = datetime.datetime.now()
        print(f'[{year}.{day:02}: {end - start}]')
        return True
    except ModuleNotFoundError:
        return False


if __name__ == "__main__":
    main()
