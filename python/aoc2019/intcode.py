import re
from collections import defaultdict


def parse_program(line):
    return list(map(int, re.findall(r'[-+]?\d+', line)))


def v1(program, overlay=None):
    memory = {pos: val for pos, val in enumerate(program)}
    if overlay:
        memory = memory | overlay
    pc = 0
    while pc in memory:
        a, b, c = memory[pc + 1], memory[pc + 2], memory[pc + 3]
        match memory[pc]:
            case 1:
                memory[c] = memory[a] + memory[b]
            case 2:
                memory[c] = memory[a] * memory[b]
            case 99:
                break
            case _ as op:
                raise ValueError(op)
        pc = pc + 4

    return memory[0]


def v2(program, *, overlay=None, read=lambda: 0, write=lambda n: print(n)):
    memory = defaultdict(int)
    memory.update({pos: val for pos, val in enumerate(program)})

    def fetch(mode, p):
        match mode:
            case 0:
                return memory[p]
            case 1:
                return p
            case _:
                raise ValueError

    if overlay is not None:
        memory = memory | overlay
    ip = 0
    jump = {1: 4, 2: 4, 3: 2, 4: 2, 5: 0, 6: 0, 7: 4, 8: 4}
    while ip in memory:
        instruction = memory[ip]
        modes, op = instruction // 100, instruction % 100
        ma, mb, mc = modes % 10, (modes // 10) % 10, (modes // 100) % 10
        pa, pb, pc = fetch(ma, ip + 1), fetch(mb, ip + 2), fetch(mc, ip + 3)
        a, b, c = memory[pa], memory[pb], memory[pc]
        match op:
            case 1:
                memory[pc] = a + b
            case 2:
                memory[pc] = a * b
            case 3:
                memory[pa] = read()
            case 4:
                write(a)
            case 5:
                if a:
                    ip = memory[pb]
                else:
                    ip = ip + 3
            case 6:
                if not a:
                    ip = memory[pb]
                else:
                    ip = ip + 3
            case 7:
                memory[pc] = a < b
            case 8:
                memory[pc] = a == b
            case 99:
                break
            case _ as op:
                raise ValueError(op)
        ip = ip + jump[op]


def v3(program, *, overlay=None, read=lambda: 0, write=lambda n: print(n)):
    memory = defaultdict(int)
    memory.update({pos: val for pos, val in enumerate(program)})
    if overlay is not None:
        memory = memory | overlay
    ip = 0
    rb = 0
    jump = {1: 4, 2: 4, 3: 2, 4: 2, 5: 0, 6: 0, 7: 4, 8: 4, 9: 2}

    def fetch(mode, p):
        nonlocal rb
        match mode:
            case 0:
                return memory[p]
            case 1:
                return p
            case 2:
                return memory[p] + rb
            case _:
                raise ValueError

    while ip in memory:
        instruction = memory[ip]
        modes, op = instruction // 100, instruction % 100
        ma, mb, mc = modes % 10, (modes // 10) % 10, (modes // 100) % 10
        pa, pb, pc = fetch(ma, ip + 1), fetch(mb, ip + 2), fetch(mc, ip + 3)
        a, b, c = memory[pa], memory[pb], memory[pc]
        match op:
            case 1:
                memory[pc] = a + b
            case 2:
                memory[pc] = a * b
            case 3:
                memory[pa] = read()
            case 4:
                write(a)
            case 5:
                if a:
                    ip = b
                else:
                    ip = ip + 3
            case 6:
                if not a:
                    ip = b
                else:
                    ip = ip + 3
            case 7:
                memory[pc] = a < b
            case 8:
                memory[pc] = a == b
            case 9:
                rb = rb + a
            case 99:
                break
            case _ as op:
                raise ValueError(op)
        ip = ip + jump[op]
