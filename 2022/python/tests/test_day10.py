from aoc2022 import day10

small_sample = ("noop\n"
                "addx 3\n"
                "addx -5")

small_sample_parsed = [None, 3, -5]

large_sample = ("addx 15\n"
                "addx -11\n"
                "addx 6\n"
                "addx -3\n"
                "addx 5\n"
                "addx -1\n"
                "addx -8\n"
                "addx 13\n"
                "addx 4\n"
                "noop\n"
                "addx -1\n"
                "addx 5\n"
                "addx -1\n"
                "addx 5\n"
                "addx -1\n"
                "addx 5\n"
                "addx -1\n"
                "addx 5\n"
                "addx -1\n"
                "addx -35\n"
                "addx 1\n"
                "addx 24\n"
                "addx -19\n"
                "addx 1\n"
                "addx 16\n"
                "addx -11\n"
                "noop\n"
                "noop\n"
                "addx 21\n"
                "addx -15\n"
                "noop\n"
                "noop\n"
                "addx -3\n"
                "addx 9\n"
                "addx 1\n"
                "addx -3\n"
                "addx 8\n"
                "addx 1\n"
                "addx 5\n"
                "noop\n"
                "noop\n"
                "noop\n"
                "noop\n"
                "noop\n"
                "addx -36\n"
                "noop\n"
                "addx 1\n"
                "addx 7\n"
                "noop\n"
                "noop\n"
                "noop\n"
                "addx 2\n"
                "addx 6\n"
                "noop\n"
                "noop\n"
                "noop\n"
                "noop\n"
                "noop\n"
                "addx 1\n"
                "noop\n"
                "noop\n"
                "addx 7\n"
                "addx 1\n"
                "noop\n"
                "addx -13\n"
                "addx 13\n"
                "addx 7\n"
                "noop\n"
                "addx 1\n"
                "addx -33\n"
                "noop\n"
                "noop\n"
                "noop\n"
                "addx 2\n"
                "noop\n"
                "noop\n"
                "noop\n"
                "addx 8\n"
                "noop\n"
                "addx -1\n"
                "addx 2\n"
                "addx 1\n"
                "noop\n"
                "addx 17\n"
                "addx -9\n"
                "addx 1\n"
                "addx 1\n"
                "addx -3\n"
                "addx 11\n"
                "noop\n"
                "noop\n"
                "addx 1\n"
                "noop\n"
                "addx 1\n"
                "noop\n"
                "noop\n"
                "addx -13\n"
                "addx -19\n"
                "addx 1\n"
                "addx 3\n"
                "addx 26\n"
                "addx -30\n"
                "addx 12\n"
                "addx -1\n"
                "addx 3\n"
                "addx 1\n"
                "noop\n"
                "noop\n"
                "noop\n"
                "addx -9\n"
                "addx 18\n"
                "addx 1\n"
                "addx 2\n"
                "noop\n"
                "noop\n"
                "addx 9\n"
                "noop\n"
                "noop\n"
                "noop\n"
                "addx -1\n"
                "addx 2\n"
                "addx -37\n"
                "addx 1\n"
                "addx 3\n"
                "noop\n"
                "addx 15\n"
                "addx -21\n"
                "addx 22\n"
                "addx -6\n"
                "addx 1\n"
                "noop\n"
                "addx 2\n"
                "addx 1\n"
                "noop\n"
                "addx -10\n"
                "noop\n"
                "noop\n"
                "addx 20\n"
                "addx 1\n"
                "addx 2\n"
                "addx 2\n"
                "addx -6\n"
                "addx -11\n"
                "noop\n"
                "noop\n"
                "noop")


def test_parse():
    assert day10.parse_lines(small_sample.splitlines()) == small_sample_parsed


def test_noop_generates_one_value():
    assert len(list(day10.cycle(None, (0, 0)))) == 1


def test_noop_increments_x_by_v():
    result = list(day10.cycle(None, (0, 10)))
    x, _ = result[0]
    assert x == 10


def test_noop_resets_v():
    result = list(day10.cycle(None, (0, 10)))
    _, v = result[0]
    assert v == 0


def test_addx_generates_two_values():
    assert len(list(day10.cycle(3, (0, 0)))) == 2


def test_addx_applies_v_once_then_0():
    result = list(day10.cycle(10, (10, 10)))
    x1, _ = result[0]
    x2, _ = result[1]

    assert x1 == 10 + 10
    assert x1 == x2


def test_addx_final_state_includes_value():
    result = list(day10.cycle(-5, (1, 0)))
    _, v = result[1]
    assert -5 == v


def test_applying_sample():
    assert list(day10.apply(small_sample_parsed)) == [1, 1, 1, 4, 4]


def test_sum_signal_strengths():
    large = day10.parse_lines(large_sample.splitlines())
    assert day10.sum_signal_strengths(large) == 13140


large_rendered = ("##  ##  ##  ##  ##  ##  ##  ##  ##  ##  \n"
                  "###   ###   ###   ###   ###   ###   ### \n"
                  "####    ####    ####    ####    ####    \n"
                  "#####     #####     #####     #####     \n"
                  "######      ######      ######      ####\n"
                  "#######       #######       #######     \n")


def test_render():
    assert day10.render(day10.parse_lines(large_sample.splitlines())) == large_rendered
