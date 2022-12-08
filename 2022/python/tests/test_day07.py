from collections import defaultdict
from pathlib import Path

import pytest
from aoc2022 import day07


sample_input = ('$ cd /\n'
                '$ ls\n'
                'dir a\n'
                '14848514 b.txt\n'
                '8504156 c.dat\n'
                'dir d\n'
                '$ cd a\n'
                '$ ls\n'
                'dir e\n'
                '29116 f\n'
                '2557 g\n'
                '62596 h.lst\n'
                '$ cd e\n'
                '$ ls\n'
                '584 i\n'
                '$ cd ..\n'
                '$ cd ..\n'
                '$ cd d\n'
                '$ ls\n'
                '4060174 j\n'
                '8033020 d.log\n'
                '5626152 d.ext\n'
                '7214296 k')

sample_expected = [
    ('cd', Path('/')),
    'ls',
    Path('a'),
    (Path('b.txt'), 14848514),
    (Path('c.dat'), 8504156),
    Path('d'),
    ('cd', Path('a')),
    'ls',
    Path('e'),
    (Path('f'), 29116),
    (Path('g'), 2557),
    (Path('h.lst'), 62596),
    ('cd', Path('e')),
    'ls',
    (Path('i'), 584),
    ('cd', Path('..')),
    ('cd', Path('..')),
    ('cd', Path('d')),
    'ls',
    (Path('j'), 4060174),
    (Path('d.log'), 8033020),
    (Path('d.ext'), 5626152),
    (Path('k'), 7214296),
]


@pytest.mark.parametrize("line, expected", zip(sample_input.strip().split('\n'), sample_expected))
def test_parse_line(line, expected):
    assert day07.parse_line(line) == expected


sample_filesystem = {
    Path('/'): {Path('a'),
                (Path('b.txt'), 14848514),
                (Path('c.dat'), 8504156),
                Path('d')},
    Path('/a'): {Path('e'),
                 (Path('f'), 29116),
                 (Path('g'), 2557),
                 (Path('h.lst'), 62596)},
    Path('/d'): {(Path('j'), 4060174),
                 (Path('d.log'), 8033020),
                 (Path('d.ext'), 5626152),
                 (Path('k'), 7214296)},
    Path('/a/e'): {(Path('i'), 584)},
}


def test_interaction_to_filesystem():
    assert day07.process_interactions(sample_expected) == sample_filesystem


sample_sizes = {
    Path('/'): 48381165,
    Path('/a'): 94853,
    Path('/d'): 24933642,
    Path('/a/e'): 584,
}


def test_collect_sizes():
    sizes = {}
    day07.collect_sizes(sample_filesystem, path=Path('/'), sizes=sizes)
    assert sizes == sample_sizes


if __name__ == '__main__':
    pytest.main()
