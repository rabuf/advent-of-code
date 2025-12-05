import datetime
import importlib
from itertools import zip_longest


def chunk(lst, size):
    args = [iter(lst)] * size
    return zip(*args)


def chunk_fill(lst, size):
    args = [iter(lst)] * size
    return zip_longest(*args)


def print_day(day, *args):
    print(f"Day {day:02}:")
    for idx, val in enumerate(args):
        print(f"\tPart {idx + 1}: {val}")


def input_to_grid(lines, translate=lambda x: x):
    grid = []
    for line in lines:
        row = []
        for c in line.strip():
            row.append(translate(c))
        grid.append(row)
    return grid


def load_and_run(year, day):
    try:
        m = importlib.import_module(f"aoc{year}.day{day:02}")
        start = datetime.datetime.now()
        if hasattr(m, "main"):
            m.main()
        end = datetime.datetime.now()
        print(f"[{year}.{day:02}: {end - start}]")
        return True
    except ModuleNotFoundError:
        return False
