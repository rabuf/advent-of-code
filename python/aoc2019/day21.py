import sys
from pathlib import Path

from aoc_util import print_day
from aoc2019 import intcode


def robot(program, instructions, output=True):
    write, read, t = intcode.run_with_queues(program)
    for i in instructions:
        for c in map(ord, i):
            write.put(c)
        write.put(10)
    t.join()
    while read.qsize() > 0:
        c = read.get()
        if c in range(0,128):
            if output:
                print(chr(c), end='')
        else:
            return c


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "21.txt") as f:
            line = f.read()
        program = intcode.parse_program(line)
        p1 = robot(program, ['NOT A J',
                             'NOT C T', 'OR T J',
                             'AND D J', 'WALK'], False)
        p2 = 0 # robot(program, ['NOT A J', 'OR D T', 'AND H T', 'OR T J', 'RUN'])
        p3 = robot(program, ['NOT A J',
                             'NOT B T', 'AND D T', 'AND H T', 'OR T J',
                             'NOT C T', 'AND D T', 'AND E T', 'OR T J',
                             'NOT C T', 'AND D T', 'AND H T', 'OR T J',
                             'AND D J',
                             'RUN'],
                   True)
        print_day("21", p1, p2, p3)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
