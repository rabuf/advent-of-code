import sys
from itertools import product
from pathlib import Path

from aoc_util import print_day


def translate_to_c(instructions, name="day24.c"):
    def recur(out, line_no, prefix="", level=1):
        while line_no < len(instructions):
            match instructions[line_no]:
                case ["inp", _]:
                    out.write("}\n")
                    out.write(f"void block{level}(int tz) {{\n")
                    out.write(prefix + "for(int w = 9; w > 0; w--) {\n")
                    out.write("int x = 0;\nint y = 0;\nint z = tz;")
                    level += 1
                case ["add", dst, src]:
                    out.write(prefix + f"{dst} = {dst} + {src};\n")
                case ["mul", dst, src]:
                    out.write(prefix + f"{dst} = {dst} * {src};\n")
                case ["div", dst, src]:
                    out.write(prefix + f"{dst} = {dst} / {src};\n")
                case ["mod", dst, src]:
                    out.write(prefix + f"{dst} = {dst} % {src};\n")
                case ["eql", dst, src]:
                    out.write(prefix + f"{dst} = ({dst} == {src}) ? 1 : 0;\n")
            line_no += 1

    with open(name, "w") as out:
        out.write("#include <stdio.h>\n")
        out.write("int main() {\n")
        out.write("   int x, y, z;\n")
        recur(out, 0, "   ")
        out.write("}\n")


def part1(instructions):
    def execute_block(w, z, line_no):
        registers = {"w": w, "z": z, "x": 0, "y": 0}
        while line_no < len(instructions):
            inst = instructions[line_no]
            match inst:
                case ["inp", _]:
                    break
                case ["add", dst, src]:
                    try:
                        src = int(src)
                    except Exception:
                        src = registers[src]
                    registers[dst] = registers[dst] + src
                case ["mul", dst, src]:
                    try:
                        src = int(src)
                    except Exception:
                        src = registers[src]
                    registers[dst] = registers[dst] * src
                case ["div", dst, src]:
                    try:
                        src = int(src)
                    except Exception:
                        src = registers[src]
                    registers[dst] = round(registers[dst] / src)
                case ["mod", dst, src]:
                    try:
                        src = int(src)
                    except Exception:
                        src = registers[src]
                    registers[dst] = registers[dst] % src
                case ["eql", dst, src]:
                    try:
                        src = int(src)
                    except Exception:
                        src = registers[src]
                    registers[dst] = 1 if registers[dst] == src else 0
            line_no += 1
        return registers["z"]

    blocks = [i + 1 for i, line in enumerate(instructions) if line[0] == "inp"]
    zs = [set() for _ in range(len(blocks))]
    for w1, w2 in product(range(1, 10), range(1, 10)):
        z1 = w1 + 2
        z2 = (w1 + 2) * 26 + w2 + 13
        z = execute_block(w1, 0, blocks[0])
        zs[0].add(z)
        assert z == z1
        z = execute_block(w2, z, blocks[1])
        zs[1].add(z)
        assert z == z2
    for z in zs:
        print(len(z), z)
    return "shit"


def parse_line(line):
    return line.split()


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2021" / "24.txt") as _:
        # lines = list(map(parse_line, f.read().splitlines()))
        p1 = 93997999296912
        p2 = 81111379141811
        print_day(24, p1, p2)
        print(
            "NOTE: Completed with C program partially generated above and then hand optimized."
        )


if __name__ == "__main__":
    main()
