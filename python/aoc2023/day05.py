import re
import sys
from pathlib import Path

import aoc_util
from aoc_util import print_day


def next_ranges(initial, ranges) -> list[range]:
    result = []
    remaining = initial
    for (r, base) in ranges.items():
        next_set = []
        for s in remaining:
            # s is entirely above or below r
            if s[-1] < r[0] or r[-1] < s[0]:
                next_set.append(s)
            # s is entirely around r
            if s[0] < r[0] and r[-1] < s[-1]:
                result.append(range(base, base + len(r)))
                next_set.append(range(s[0], r[0]))
                next_set.append(range(r[-1]+1, s[-1]+1))
            # s is entirely within r
            if r[0] <= s[0] and s[-1] <= r[-1]:
                start = s[0] + base - r[0]
                result.append(range(start, start + len(s)))
            # s overlaps from below
            if s[0] < r[0] <= s[-1] <= r[-1]:
                next_set.append(range(s[0], r[0]))
                start = base
                end = base + s[-1] - r[0] + 1
                result.append(range(start, end))
            # s overlaps from above
            if r[0] <= s[0] <= r[-1] < s[-1]:
                next_set.append(range(r[-1] + 1, s[-1] + 1))
                start = base + s[0] - r[0]
                end = base + len(r)
                result.append(range(start, end))
        remaining = next_set
    result.extend(remaining)
    return result


def create_map(ranges):
    lines = ranges.splitlines()
    name = lines[0].split()[0].split('-to-')
    section_map = {}
    for line in lines[1:]:
        parts = [int(part) for part in line.split()]
        section_map[range(parts[1], parts[1]+parts[2])] = parts[0]
    return name, section_map


def determine_location_ranges(seed_ranges, maps, resource_maps) -> list[range]:
    resource_type = 'seed'
    ranges = seed_ranges
    while resource_type != 'location':
        ranges = next_ranges(ranges, maps[resource_type])
        resource_type = resource_maps[resource_type]
    return ranges


def determine_location(seed, maps, resource_maps) -> int:
    value = seed
    resource_type = 'seed'
    while resource_type != 'location':
        for (key, base) in maps[resource_type].items():
            if value in key:
                value = base + value - key[0]
                break
        resource_type = resource_maps[resource_type]

    return value


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "05.txt") as f:
        sections = f.read().split('\n\n')
    seeds = [int(seed) for seed in re.findall(r'\d+', sections[0])]
    parsed = [create_map(section) for section in sections[1:]]
    resource_types = {name[0]: name[1] for (name, _) in parsed}
    maps = {name[0]: ranges for (name, ranges) in parsed}
    locations = [determine_location(seed, maps, resource_types) for seed in seeds]
    seed_ranges = [range(start, start + length) for (start, length) in aoc_util.chunk(seeds, 2)]
    p2_ranges = determine_location_ranges(seed_ranges, maps, resource_types)
    p2 = min(r[0] for r in p2_ranges)
    print_day(5, min(locations), p2)


if __name__ == '__main__':
    main()
