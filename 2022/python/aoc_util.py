def chunk(lst, size):
    args = [iter(lst)] * size
    return zip(*args)


def print_day(day, *args):
    print(f'Day {day:02}:')
    for idx, val in enumerate(args):
        print(f'\tPart {idx + 1}: {val}')
