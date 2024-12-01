from aoc2023.day20 import *
from pytest import mark, fixture


sample_1 = """broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
"""


sample_2 = """broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
"""


@fixture
def sample_2_modules():
    modules = list(map(parse_line, sample_2.splitlines()))
    return process_module_list(modules)


def test_lowest_presses(sample_2_modules):
    assert lowest_presses(sample_2_modules, sink='output') == 1


@fixture
def sample_1_modules():
    modules = list(map(parse_line, sample_1.splitlines()))
    return process_module_list(modules)


@fixture
def basic_flipflop():
    return FlipFlop('a', ['b', 'c'])


@fixture
def basic_conjunction():
    c = Conjunction('a', ['b'])
    c.add_source('f')
    c.add_source('e')
    return c


def test_button_press(sample_1_modules):
    assert press_button(sample_1_modules) == (8, 4)


def test_button_press_1000(sample_1_modules):
    assert press_button(sample_1_modules, times=1000) == (8000, 4000)


def test_conjunction_sends_high_if_any_low(basic_conjunction):
    assert basic_conjunction.apply('f', Pulse.LOW) == [('b', 'a', Pulse.HIGH)]


def test_conjunction_sends_low_if_all_high(basic_conjunction):
    basic_conjunction.apply('e', Pulse.HIGH)
    assert basic_conjunction.apply('f', Pulse.HIGH) == [('b', 'a', Pulse.LOW)]


def test_flipflop_starts_off(basic_flipflop):
    assert basic_flipflop._state == State.OFF


def test_flipflop_ignores_high_when_off(basic_flipflop):
    assert basic_flipflop.apply('c', Pulse.HIGH) == []


def test_flipflop_ignores_high_when_on(basic_flipflop):
    basic_flipflop.apply('c', Pulse.LOW)
    assert basic_flipflop.apply('c', Pulse.HIGH) == []


def test_flipflop_sends_high_from_off(basic_flipflop):
    assert basic_flipflop.apply('c', Pulse.LOW) == [('b', 'a', Pulse.HIGH), ('c', 'a', Pulse.HIGH)]


def test_flipflop_sends_high_from_on(basic_flipflop):
    basic_flipflop.apply('c', Pulse.LOW)
    assert basic_flipflop.apply('c', Pulse.LOW) == [('b', 'a', Pulse.LOW), ('c', 'a', Pulse.LOW)]