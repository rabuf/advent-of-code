### AoC 2022 - Python Edition

#### Day 1

I didn't really do anything interesting here.
Read, sort, grab the first or first three elements.

#### Day 2

Playing around with TDD and PBT. It's a trivial problem so it's a bit difficult to come up with properties, but I found
a few. Nearly every line of code is the consequence of a test, including having it fail first because the function
hasn't been defined. A more extreme form of TDD than I normally use, but a fun exercise (for me).

Scoring lent itself well to PBT. I made these tests in succession:

```python
@given(st.integers(min_value=1, max_value=3))
def test_score_equals_play_on_loss(self, opp):
    opp = RPS(opp)
    loss = a_beats_b[opp]
    self.assertEqual(loss, score(opp, loss))


@given(st.integers(min_value=1, max_value=3))
def test_score_equals_play_plus_three_on_draw(self, opp):
    opp = RPS(opp)
    tie = opp
    self.assertEqual(tie + 3, score(opp, tie))


@given(st.integers(min_value=1, max_value=3))
def test_score_equals_play_plus_six_on_win(self, opp):
    opp = RPS(opp)
    win = a_beats_b[a_beats_b[opp]]
    self.assertEqual(win + 6, score(opp, win
```

I created a similar set of tests for the strategy function.

```python
@given(st.integers(min_value=1, max_value=3))
def test_strategy_rock_loses(self, opp):
    opp = RPS(opp)
    play = strategy(opp, RPS.ROCK)
    self.assertTrue(beats(opp, play))


@given(st.integers(min_value=1, max_value=3))
def test_strategy_paper_ties(self, opp):
    opp = RPS(opp)
    play = strategy(opp, RPS.PAPER)
    self.assertEqual(opp, play)


@given(st.integers(min_value=1, max_value=3))
def test_strategy_scissors_wins(self, opp):
    opp = RPS(opp)
    play = strategy(opp, RPS.SCISSORS)
    self.assertTrue(beats(play, opp))
```

Each test resulted in a small change to the `strategy` function so it eventually handled all three cases.

#### Day 3

Going to attempt a TDD style again. Switching to pytest from unittest for the test framework. This will be an overkill
solution but, why not?

More fun with testing. In switching to pytest, I used one of its decorators to create table driven tests. Could be
cleaned up more, but it works. 