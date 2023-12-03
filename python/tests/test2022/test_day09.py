import pytest
from hypothesis.stateful import RuleBasedStateMachine, rule, invariant, initialize, precondition

from aoc2022 import day09

small_sample = ("R 4\n"
                "U 4\n"
                "L 3\n"
                "D 1\n"
                "R 4\n"
                "D 1\n"
                "L 5\n"
                "R 2")
small_dirs = [(1, 4),
              (1j, 4),
              (-1, 3),
              (-1j, 1),
              (1, 4),
              (-1j, 1),
              (-1, 5),
              (1, 2),
              ]

big_sample = ("R 5\n"
              "U 8\n"
              "L 8\n"
              "D 3\n"
              "R 17\n"
              "D 10\n"
              "L 25\n"
              "U 20\n")


@pytest.mark.parametrize('line, expected', zip(small_sample.splitlines(), small_dirs))
def test_parse_line(line, expected):
    assert day09.parse_line(line) == expected


class SnakeGame(RuleBasedStateMachine):
    """A state machine for the snake game. Using stateful testing to discover errors in the movement function"""

    snake = None
    old = None

    @staticmethod
    def touching(a, b):
        return int(a.real - b.real) in [1, 0, -1] and int(a.imag - b.imag) in [1, 0, -1]

    @staticmethod
    def in_line(a, b):
        return (a.real == b.real and a.imag != b.imag) or (a.real != b.real and a.imag == b.imag)

    @initialize()
    def set_snake(self):
        self.snake = [0, 0]
        self.old = [0, 0]

    @invariant()
    def always_touching(self):
        """This test ensures that after movement the tail is within one of the head."""
        assert SnakeGame.touching(self.snake[0], self.snake[1])

    @precondition(lambda self: SnakeGame.touching(self.old[1], self.snake[0]))
    @invariant()
    def tail_does_not_move_if_head_stays_near(self):
        assert self.old[1] == self.snake[1]

    @precondition(lambda self: not SnakeGame.touching(self.old[1], self.snake[0]))
    @invariant()
    def tail_moves_if_head_goes_too_far(self):
        assert self.old[1] != self.snake[1]

    @precondition(lambda self: (not SnakeGame.touching(self.old[1], self.snake[0])
                                and SnakeGame.in_line(self.old[1], self.snake[0])))
    @invariant()
    def tail_moves_in_line(self):
        assert SnakeGame.in_line(self.old[1], self.snake[1])

    @precondition(lambda self: (not SnakeGame.touching(self.old[1], self.snake[0])
                                and not SnakeGame.in_line(self.old[1], self.snake[0])))
    @invariant()
    def tail_moves_diagonal(self):
        assert not SnakeGame.in_line(self.old[1], self.snake[1])

    @rule()
    def move_up(self):
        self.old = self.snake.copy()
        self.snake[0] += 1j
        day09.move(self.snake)

    @rule()
    def move_down(self):
        self.old = self.snake.copy()
        self.snake[0] -= 1j
        day09.move(self.snake)

    @rule()
    def move_right(self):
        self.old = self.snake.copy()
        self.snake[0] += 1
        day09.move(self.snake)

    @rule()
    def move_left(self):
        self.old = self.snake.copy()
        self.snake[0] -= 1
        day09.move(self.snake)


TestMoveTail = SnakeGame().TestCase


def test_small_sample():
    locs = day09.part_1(small_dirs)
    print(locs)
    assert 13 == len(locs[1])


if __name__ == '__main__':
    print(pytest.__version__)
    pytest.main()
