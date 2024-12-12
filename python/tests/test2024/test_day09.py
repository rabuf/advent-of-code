import pytest

from aoc2024.day09 import *


@pytest.fixture()
def sample():
    return parse_line('2333133121414131402')


def test_part1(sample):
    compacted = compact(sample)
    assert 1928 == checksum(compacted)


def test_part2(sample):
    files, free = to_file_blocks('2333133121414131402')
    sample_compacted = compact_whole_files(sample, files, free)
    assert 2858 == checksum(sample_compacted)
