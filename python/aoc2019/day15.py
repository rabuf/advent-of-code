import sys
from pathlib import Path
from queue import Queue

import networkx

from aoc2019 import intcode
from aoc_util import print_day


def parse_line(line):
    return line


def search(read: Queue, write: Queue, G: networkx.Graph):
    walls = set()
    oxygen = 0

    def recur(pos):
        nonlocal oxygen
        for i, d, r in zip((1, 2, 3, 4), (1j, -1j, -1, 1), (2, 1, 4, 3)):
            if pos + d not in G and pos + d not in walls:
                write.put(i)
                match read.get():
                    case 0:
                        walls.add(pos + d)
                    case 1:
                        G.add_edge(pos, pos + d)
                        recur(pos + d)
                        write.put(r)
                        read.get()
                    case 2:
                        G.add_edge(pos, pos + d)
                        oxygen = pos + d
                        recur(pos + d)
                        write.put(r)
                        read.get()

    recur(0)
    return oxygen


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "15.txt") as f:
            program = intcode.parse_program(f.read())

        write, read, t = intcode.run_with_queues(program, daemon=True)
        G = networkx.Graph()
        oxygen = search(read, write, G)
        p1 = networkx.shortest_path_length(G, 0, oxygen)
        distances = networkx.single_source_shortest_path_length(G, oxygen)
        p2 = max([v for _, v in distances.items()])
        print_day("15", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
