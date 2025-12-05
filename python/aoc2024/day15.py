import sys
from pathlib import Path

from aoc_util import print_day


def to_grid(lines):
    grid = {}
    start = 0
    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            grid[i + j * 1j] = c
            if c == "@":
                start = i + j * 1j
    return grid, start


def gps_sum(grid, box="O"):
    result = sum(int(p.real) + 100 * int(p.imag) for p, c in grid.items() if c == box)
    return result


def print_grid(grid):
    j = 0
    while j in grid:
        p = j
        while p in grid:
            print(grid[p], end="")
            p = p + 1
        j = j + 1j
        print()


def widen_grid(grid):
    wide = {}
    for p, c in grid.items():
        l, r = (  # noqa: E741
            int(p.real) * 2 + int(p.imag) * 1j,
            int(p.real) * 2 + 1 + int(p.imag) * 1j,
        )
        match c:
            case "#" | ".":
                wide[l] = c
                wide[r] = c
            case "@":
                wide[l] = c
                wide[r] = "."
            case "O":
                wide[l] = "["
                wide[r] = "]"
    return wide


def apply_moves(grid, moves, start):
    grid = grid.copy()
    directions = {"^": -1j, "v": 1j, "<": -1, ">": 1}
    p = start
    for m in moves:
        d = directions[m]
        match grid[p + d]:
            case ".":
                grid[p + d], grid[p] = grid[p], grid[p + d]
                p = p + d
            case "[" | "]":
                match m:
                    case "<" | ">":
                        n = p + d
                        while grid[n] in "[]":
                            n = n + d
                        if grid[n] == ".":
                            while n != p:
                                grid[n], grid[n - d] = grid[n - d], grid[n]
                                n = n - d
                            p = p + d
                    case "^" | "v":
                        to_move = {p}
                        swaps = [(p, p + d)]
                        hit_a_wall = False
                        while not hit_a_wall and to_move:
                            s = set()
                            for n in {r + d for r in to_move}:
                                if grid[n] != ".":
                                    if (n, n + d) not in swaps:
                                        swaps.append((n, n + d))
                                    s.add(n)
                                    match grid[n]:
                                        case "[":
                                            if (n + 1, n + d + 1) not in swaps:
                                                swaps.append((n + 1, n + d + 1))
                                            s.add(n + 1)
                                        case "]":
                                            if (n - 1, n + d - 1) not in swaps:
                                                swaps.append((n - 1, n + d - 1))
                                            s.add(n - 1)
                                        case "#":
                                            hit_a_wall = True
                                            break
                                        case ".":
                                            s.add(n)
                                to_move = s
                        if not hit_a_wall:
                            for l, r in reversed(swaps):  # noqa: E741
                                grid[l], grid[r] = grid[r], grid[l]
                            p = p + d
            case "O":
                n = p + d
                while grid[n] == "O":
                    n = n + d
                if grid[n] == ".":
                    grid[n], grid[p], grid[p + d] = grid[p + d], grid[n], grid[p]
                    p = p + d
    return grid


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2024" / "15.txt") as f:
            lines = f.read()
            grid, moves = lines.split("\n\n")
            grid = grid.splitlines()
            grid, start = to_grid(grid)
            moves = "".join(moves.splitlines())
            # Part 1
            p1_grid = apply_moves(grid, moves, start)
            # Part 2
            wide_grid = widen_grid(grid)
            wide_start, *_ = (p for p in wide_grid if wide_grid[p] == "@")
            p2_grid = apply_moves(wide_grid, moves, wide_start)
            print_day(15, gps_sum(p1_grid), gps_sum(p2_grid, "["))
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
