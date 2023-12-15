from aoc2023.day15 import *


sample = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"


def test_hash():
    assert hash_algorithm('HASH') == 52


def test_part1():
    assert sum(hash_algorithm(i) for i in sample.split(',')) == 1320


def test_hash_map():
    instructions = sample.split(',')
    m = hash_map(instructions)
    total = focusing_power(m)
    assert total == 145