import sys
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    result = []
    position = 0
    for i, length in enumerate(line):
        l = int(length)
        if i % 2 == 0:
            result[position:position + l] = [i // 2] * l
        else:
            result[position:position + l] = [None] * l
        position += l
    return result


def compact(files: list):
    compacted = files[:]
    l, r = 0, len(files) - 1
    while l < r:
        while compacted[r] is None:
            r = r - 1
        while compacted[l] is not None:
            l = l + 1
        if l < r:
            compacted[l], compacted[r] = compacted[r], compacted[l]
    return compacted


def checksum(files: list):
    return sum(i * file for i, file in enumerate(files) if file is not None)


def compact_whole_files(disk, file_blocks, free_blocks: list[tuple[int, int]]):
    compacted = disk[:]
    for (file_number, start, length) in reversed(file_blocks):
        for i, (p, l) in enumerate(free_blocks):
            if p > start:
                break
            if l >= length:
                compacted[p:p + length], compacted[start:start + length] = compacted[start:start + length], compacted[
                                                                                                            p:p + length]
                if length < l:
                    free_blocks[i] = (p + length, l - length)
                else:
                    free_blocks.pop(i)
                break
    return compacted


def to_file_blocks(line):
    blocks = []
    free = []
    position = 0
    for i, length in enumerate(line):
        l = int(length)
        if i % 2 == 0:
            blocks.append((i // 2, position, l))
            position = position + l
        else:
            free.append((position, l))
            position = position + l
    return blocks, free


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2024" / "09.txt") as f:
        line = f.readline().strip()
    files = parse_line(line)
    p1_compacted = compact(files)
    p1 = checksum(p1_compacted)
    blocks, free = to_file_blocks(line)
    p2_compacted = compact_whole_files(files, blocks, free)
    p2 = checksum(p2_compacted)
    print_day(9, p1, p2)
    assert p2 == 6377400869326


if __name__ == '__main__':
    main()
