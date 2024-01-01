import datetime
import importlib


def main():
    for year in range(2022, 2024):
        year_start = datetime.datetime.now()
        for day in range(1, 26):
            load_and_run(year, day)
        year_end = datetime.datetime.now()
        print(f'[{year}: {year_end - year_start}]')


def load_and_run(year, day):
    try:

        m = importlib.import_module(f'aoc{year}.day{day:02}')
        print(m.__file__)
        start = datetime.datetime.now()
        if hasattr(m, 'main'):
            m.main()
        end = datetime.datetime.now()
        print(f'[{year}.{day:02}: {end - start}]')
    except ModuleNotFoundError:
        pass


if __name__ == "__main__":
    main()
