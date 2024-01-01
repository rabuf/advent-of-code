import datetime
import importlib


def main():
    for year in range(2022, 2024):
        year_start = datetime.datetime.now()
        for day in range(1, 25):
            try:
                m = importlib.import_module(f'aoc{year}.day{day:02}')
                start = datetime.datetime.now()
                m.main()
                end = datetime.datetime.now()
                print(f'[{year}.{day:02}: {end - start}]')
            except ModuleNotFoundError:
                pass
        year_end = datetime.datetime.now()
        print(f'[{year}: {year_end-year_start}]')


if __name__ == "__main__":
    main()
