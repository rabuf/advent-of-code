import heapq
import sys
from pathlib import Path

import regex as re
from z3 import IntVector, Optimize, sat

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
    button_vars = {}
    presses = IntVector("p", len(buttons))
    for i, button in enumerate(buttons):
        button_vars[i] = IntVector(f"b{i}", len(joltages))
        for b in range(len(joltages)):
            if b in button:
                s.add(button_vars[i][b] == 1)
            else:
                s.add(button_vars[i][b] == 0)
        s.add(presses[i] >= 0)
    for b, j in enumerate(joltages):
        s.add(sum(presses[i] * button_vars[i][b] for i in button_vars) == j)
    s.minimize(sum(presses))
    if s.check() == sat:
        m = s.model()
        return sum(m[p].as_long() for p in presses)
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
