import sys
from collections import defaultdict
from pathlib import Path

from aoc_util import print_day


def parse_command(command):
    match command.split():
        case [_, 'cd', directory]:
            return 'cd', Path(directory)
        case [_, 'ls']:
            return 'ls'


def parse_listing(listing):
    match listing.split():
        case ['dir', directory]:
            return Path(directory)
        case [size, name]:
            return Path(name), int(size)


def parse_line(line):
    match line.split():
        case ['$', *rest]:
            return parse_command(line)
        case _:
            return parse_listing(line)


def process_interactions(interactions):
    filesystem = defaultdict(set)
    pwd = Path()
    for interaction in interactions:
        match interaction:
            case ('cd', '/'):
                pwd = Path('/')
            case ('cd', path):
                pwd = pwd / path
                pwd = pwd.resolve()
            case Path() as path:
                filesystem[pwd].add(path)
            case (Path() as path, int() as size):
                filesystem[pwd].add((path, size))
    return filesystem


def collect_sizes(filesystem, path=Path('/'), sizes=None):
    if sizes is None:
        sizes = defaultdict(int)
    if path not in sizes:
        sizes[path] = 0
    for listing in filesystem[path]:
        match listing:
            case Path():
                collect_sizes(filesystem, path / listing, sizes)
                sizes[path] += sizes[path / listing]
            case (Path(), int() as size):
                sizes[path] += size
    return sizes


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2022" / "07.txt") as f:
        interactions = [parse_line(line) for line in f.readlines()]
        filesystem = process_interactions(interactions)
        sizes = collect_sizes(filesystem)
        under_limit = sum(size for size in sizes.values() if size <= 100000)
        available = 70000000
        needed = 30000000
        limit = sizes[Path('/')] - (available - needed)
        to_remove = min(*[size for size in sizes.values() if size >= limit])
        print_day(7, under_limit, to_remove)


if __name__ == '__main__':
    print(sys.argv[1])
    main()
