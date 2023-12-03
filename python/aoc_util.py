from itertools import zip_longest


def chunk(lst, size):
    args = [iter(lst)] * size
    return zip(*args)


def chunk_fill(lst, size):
    args = [iter(lst)] * size
    return zip_longest(*args)


def print_day(day, *args):
    print(f'Day {day:02}:')
    for idx, val in enumerate(args):
        print(f'\tPart {idx + 1}: {val}')


def input_to_grid(lines, translate=lambda x: x):
    grid = []
    for line in lines:
        row = []
        for c in line.strip():
            row.append(translate(c))
        grid.append(row)
    return grid
