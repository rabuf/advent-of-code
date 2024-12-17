import re
import sys
from pathlib import Path

from aoc_util import print_day


def simulate(a, b, c, program):
    ip = 0
    output = []
    def combo(opcode):
        match opcode:
            case 0 | 1 | 2 | 3:
                return opcode
            case 4:
                return a
            case 5:
                return b
            case 6:
                return c
            case 7:
                raise ValueError
    while ip in range(len(program)):
        operand = program[ip + 1]
        match program[ip]:
            case 0: # adv
                a = a // (2 ** combo(operand))
                ip = ip + 2
            case 1: # bxl
                b = b ^ operand
                ip = ip + 2
            case 2: # bst
                b = combo(operand) % 8
                ip = ip + 2
            case 3: # jnz
                if a != 0:
                    ip = operand
                else:
                    ip = ip + 2
            case 4: # bxc
                b = b ^ c
                ip = ip + 2
            case 5: # out
                output.append(combo(operand) % 8)
                ip = ip + 2
            case 6: # bdv
                b = a // (2 ** combo(operand))
                ip = ip + 2
            case 7: # cdv
                c = a // (2 ** combo(operand))
                ip = ip + 2
    return output


def translated(a):
    out = []
    b, c = 0, 0
    # 3 0
    while True:
        # 2 4
        b = a % 8
        # 1 1
        b = b ^ 1
        # 7 5
        c = a // (2 ** b)
        # 1 5
        b = b ^ 5
        # 4 2
        b = b ^ c
        # 5 5
        out.append(b % 8)
        # 0 3
        a = a // (2 ** 3)
        if a == 0:
            break
    return out


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2024" / "17.txt") as f:
            a, b, c, *program = map(int, re.findall(r'\d+', f.read()))
        p1 = ','.join(map(str, simulate(a, b, c, program)))
        parts = [4, 5, 2, 6, 4, 4, 2, 1, 3, 3, 2, 6, 7, 2, 7, 5]
        n = 0
        for i in parts:
            n = n * 8 + i
        p2 = n
        result = simulate(p2, 0, 0, program)
        assert result == program
        print_day("17", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
