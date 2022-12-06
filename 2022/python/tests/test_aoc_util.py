from hypothesis import given, strategies as st

from aoc_util import *


@given(st.lists(st.just(1), min_size=1), st.integers(min_value=1, max_value=5))
def test_chunk_size_is_chunk_size(lst, chunk_size):
    chunked = chunk(lst, chunk_size)
    for c in chunked:
        assert len(c) == chunk_size


@given(st.lists(st.just(1), min_size=3), st.integers(min_value=1, max_value=5))
def test_number_chunks_is_floor_input_div_size(lst, chunk_size):
    assert len(lst) // chunk_size == len(list(chunk(lst, chunk_size)))
