import sys
from pathlib import Path

import networkx

from aoc2019 import intcode
from aoc_util import print_day


def parse_line(line):
    return line


def main(input_dir=Path(sys.argv[1])):
    try:
        with open(input_dir / "2019" / "17.txt") as f:
            lines = f.read()
        program = intcode.parse_program(lines)
        output = []
        intcode.v3(program, write=lambda n: output.append(chr(n)))
        lines = "".join(output).splitlines()
        G = networkx.Graph()
        for i, line in enumerate(lines):
            for j, c in enumerate(line):
                p = complex(j, i)
                if c in "#^":
                    G.add_node(p)
                    for n in (p + d for d in (1, -1, 1j, -1j)):
                        if n in G:
                            G.add_edge(p, n)
        p1 = sum(int(n.real * n.imag) for n in G if G.degree[n] == 4)
        print_day("17", p1)
    except IOError as e:
        print(e)


if __name__ == "__main__":
    main()
