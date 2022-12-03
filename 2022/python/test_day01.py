from unittest import TestCase

from hypothesis import given, strategies as st

import day01


class Test(TestCase):
    sample = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""

    @given(st.lists(st.integers(), min_size=1))
    def test_solve(self, elves):
        elves = sorted(elves)
        a, b = day01.solve(elves)
        self.assertEqual(a, max(elves))
        self.assertEqual(b, sum(elves[-3:]))

    def test_sample(self):
        elves = day01.parse(self.sample)
        self.assertListEqual(elves, [4000, 6000, 10000, 11000, 24000])
        a, b = day01.solve(elves)
        self.assertEqual(a, 24000)
        self.assertEqual(b, 45000)
