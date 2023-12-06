from hypothesis import given, strategies as st, assume

from aoc2023.day06 import *


@given(time=st.integers(min_value=1, max_value=100_000), distance=st.integers(min_value=0, max_value=100_000))
def test_approaches(time, distance):
    # if this value is negative, then we end up with a complex number
    # when calculating the sqrt, these are treated as invalid inputs
    # by calling `assume`, rather than causing a test failure.
    assume(0 <= time ** 2 - 4 * distance)
    assert fast_ways_to_beat(time, distance) == ways_to_beat(time, distance)
