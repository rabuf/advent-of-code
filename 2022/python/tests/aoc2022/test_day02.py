from unittest import TestCase

from hypothesis import given, strategies as st

from aoc2022.day02 import *


class Test(TestCase):
    # Custom strategy for generating RPS play. Used by hypothesis tests.
    rps_play = st.integers(min_value=1, max_value=3).map(RPS)

    def test_values(self):
        self.assertEqual(1, RPS.ROCK)
        self.assertEqual(2, RPS.PAPER)
        self.assertEqual(3, RPS.SCISSORS)

    def test_beats(self):
        self.assertTrue(beats(RPS.ROCK, RPS.SCISSORS))
        self.assertTrue(beats(RPS.PAPER, RPS.ROCK))
        self.assertTrue(beats(RPS.SCISSORS, RPS.PAPER))

    def test_parse_line(self):
        self.assertEqual((RPS.ROCK, RPS.ROCK), parse_line("A X"))
        self.assertEqual((RPS.PAPER, RPS.PAPER), parse_line("B Y"))
        self.assertEqual((RPS.SCISSORS, RPS.SCISSORS), parse_line("C Z"))

    @given(rps_play)
    def test_score_equals_play_on_loss(self, opp):
        loss = a_beats_b[opp]
        self.assertEqual(loss, score(opp, loss))

    @given(rps_play)
    def test_score_equals_play_plus_three_on_draw(self, opp):
        tie = opp
        self.assertEqual(tie + 3, score(opp, tie))

    @given(rps_play)
    def test_score_equals_play_plus_six_on_win(self, opp):
        win = a_beats_b[a_beats_b[opp]]
        self.assertEqual(win + 6, score(opp, win))

    sample = "A Y\nB X\nC Z"
    sample_expected = [(RPS.ROCK, RPS.PAPER), (RPS.PAPER, RPS.ROCK), (RPS.SCISSORS, RPS.SCISSORS)]

    def test_parse(self):
        parsed = parse(self.sample.split("\n"))
        for actual, expected in zip(parsed, self.sample_expected):
            self.assertEqual(actual, expected)

    def test_solve(self):
        parsed = parse(self.sample.split("\n"))
        a, b = solve(parsed)
        self.assertEqual(a, 15)
        self.assertEqual(b, 12)

    @given(rps_play)
    def test_strategy_rock_loses(self, opp):
        play = strategy(opp, RPS.ROCK)
        self.assertTrue(beats(opp, play))

    @given(rps_play)
    def test_strategy_paper_ties(self, opp):
        play = strategy(opp, RPS.PAPER)
        self.assertEqual(opp, play)

    @given(rps_play)
    def test_strategy_scissors_wins(self, opp):
        play = strategy(opp, RPS.SCISSORS)
        self.assertTrue(beats(play, opp))
