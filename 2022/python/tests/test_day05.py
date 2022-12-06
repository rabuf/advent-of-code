import pytest

from aoc2022 import day05

sample_input = ("    [D]    \n"
                "[N] [C]    \n"
                "[Z] [M] [P]\n"
                " 1   2   3 \n"
                "\n"
                "move 1 from 2 to 1\n"
                "move 3 from 1 to 3\n"
                "move 2 from 2 to 1\n"
                "move 1 from 1 to 2")

sample_crates = {
    1: ['Z', 'N'],
    2: ['M', 'C', 'D'],
    3: ['P'],
}

sample_raw_crates, sample_moves = sample_input.split('\n\n')
sample_raw_crates = sample_raw_crates.split('\n')
sample_moves = sample_moves.split('\n')
sample_moves_expected = [(1, 2, 1), (3, 1, 3), (2, 2, 1), (1, 1, 2)]


@pytest.mark.parametrize("move, expected", zip(sample_moves, sample_moves_expected))
def test_parse_move(move, expected):
    assert day05.parse_move(move) == expected


def test_parse_crates():
    crates = day05.parse_crates(sample_raw_crates)
    assert crates == sample_crates


def test_parse():
    crates, moves = day05.parse(sample_input)
    assert crates == sample_crates
    assert moves == sample_moves_expected


def test_crate_hash():
    assert day05.crate_hash(sample_crates) == "NDP"


# Using `parse_crates` since it's been tested at this point
sample_target = ("        [Z]\n"
                 "        [N]\n"
                 "        [D]\n"
                 "[C] [M] [P]\n"
                 " 1   2   3")
sample_target_crates = day05.parse_crates(sample_target.split('\n'))


def test_apply_moves():
    crates, moves = day05.parse(sample_input)
    day05.apply_moves(crates, moves)
    assert crates == sample_target_crates


sample_target_9001 = ("        [D]\n"
                      "        [N]\n"
                      "        [Z]\n"
                      "[M] [C] [P]\n"
                      " 1   2   3")
sample_target_crates_9001 = day05.parse_crates(sample_target_9001.split('\n'))


def test_apply_moves_9001():
    crates, moves = day05.parse(sample_input)
    day05.apply_moves_9001(crates, moves)
    assert crates == sample_target_crates_9001


if __name__ == '__main__':
    pytest.main()
