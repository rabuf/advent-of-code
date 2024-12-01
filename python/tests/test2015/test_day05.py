import unittest
from aoc2018.day05 import reactp

from hypothesis import given, strategies as st


@given(st.characters(max_codepoint=127, categories=['Ll']))
def test_react_matches_miscased_pairs(c):
    assert reactp(c, c.upper())


@given(st.characters(max_codepoint=127, categories=['Ll', 'Lu']))
def test_react_does_not_match_same_cased_pairs(c):
    assert not reactp(c, c)


@st.composite
def distinct_characters(draw):
    a = draw(st.characters(max_codepoint=127, categories=['Ll', 'Lu']))
    b = draw(st.characters(max_codepoint=127, categories=['Ll', 'Lu'], exclude_characters=a + a.upper() + a.lower()))
    return a, b


@given(distinct_characters())
def test_react_does_not_match_different_pairs(c):
    a, b = c
    assert not reactp(a, b)
