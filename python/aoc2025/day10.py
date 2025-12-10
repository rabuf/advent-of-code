import heapq
import sys
from collections import defaultdict
from pathlib import Path

import regex as re
from z3 import Int, Optimize, sat

from aoc_util import print_day

YEAR = "2025"
DAY = "10"


def state_to_string(state):
    return "".join("#" if s else "." for s in state)


def parse_line(line):
    target_pattern = r"\[((\.|#)*)\]"
    target = re.match(target_pattern, line).group(1)
    rest = line[len(target) + 3 :].split(" ")
    buttons = [tuple(map(int, re.findall(r"\d+", button))) for button in rest[:-1]]
    joltages = tuple(map(int, rest[-1].strip("{}").split(",")))

    return target, buttons, joltages


def solve1(target, buttons):
    target = [t == "#" for t in target]
    queue = [(0, [False] * len(target))]
    heapq.heapify(queue)
    while True:
        presses, state = heapq.heappop(queue)
        if state == target:
            return presses
        for button in buttons:
            next = state[:]
            for b in button:
                next[b] = not next[b]
            if (presses + 1, next) not in queue:
                heapq.heappush(queue, (presses + 1, next))
    return 0


def part1(schematics):
    return sum(solve1(target, buttons) for target, buttons, _ in schematics)


def solve2_z3(buttons, joltages):
    s = Optimize()
    button_vars = defaultdict(dict)
    presses = {}
    for i, button in enumerate(buttons):
        for b in range(len(joltages)):
            button_vars[b][i] = Int(f"b{i}{b}")
            if b in button:
                s.add(button_vars[b][i] == 1)
            else:
                s.add(button_vars[b][i] == 0)
        presses[i] = Int(f"p{i}")
        s.add(presses[i] >= 0)
    for b, j in enumerate(joltages):
        s.add(sum(presses[i] * button_vars[b][i] for i in presses) == j)
    s.minimize(sum(presses.values()))
    if s.check() == sat:
        m = s.model()
        return sum(m.evaluate(p).as_long() for p in presses.values())
    else:
        print("No solution?")
        print(s)
        return 0


def part2(schematics):
    return sum(solve2_z3(buttons, joltages) for _, buttons, joltages in schematics)


SAMPLE = """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"""


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / YEAR / f"{DAY}.txt") as f:
            lines = list(map(parse_line, f.read().splitlines()))
        sample = list(map(parse_line, SAMPLE.splitlines()))
        print_day(DAY, part1(lines), part2(lines))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
