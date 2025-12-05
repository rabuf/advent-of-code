import sys
from collections import defaultdict
from pathlib import Path

from aoc2019 import intcode
from aoc_util import print_day


def parse_line(line):
    return line


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2019" / "11.txt") as f:
            program = intcode.parse_program(f.read())
        grid = defaultdict(int)
        d = 1j
        pos = 0
        in_queue, out_queue, t = intcode.run_with_queues(program)
        while t.is_alive():
            in_queue.put(grid[pos])
            color, direction = out_queue.get(), out_queue.get()
            grid[pos] = color
            d = d * (-1j if direction else 1j)
            pos = pos + d
        p1 = len(grid)
        grid = defaultdict(int)
        d = 1j
        pos = 0
        grid[pos] = 1
        nw, se = [0, 0], [0, 0]
        in_queue, out_queue, t = intcode.run_with_queues(program)
        while t.is_alive():
            in_queue.put(grid[pos])
            color, direction = out_queue.get(), out_queue.get()
            grid[pos] = color
            d = d * (-1j if direction else 1j)
            nw[0] = int(min(nw[0], pos.real))
            nw[1] = int(max(nw[1], pos.imag))

            se[0] = int(max(se[0], pos.real))
            se[1] = int(min(se[1], pos.imag))
            pos = pos + d
        for j in range(nw[1], se[1] - 1, -1):
            for i in range(nw[0], se[0]):
                print("#" if grid[i + j * 1j] else " ", end="")
            print()
        print_day("11", p1, "EGHKGJER")
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
