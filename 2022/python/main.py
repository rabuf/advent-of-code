import importlib


def main():
    for year in range(2015, 2023):
        for day in range(1, 26):
            try:
                m = importlib.import_module(f'aoc{year}.day{day:02}')
                m.main()
            except ModuleNotFoundError:
                pass


if __name__ == "__main__":
    main()
