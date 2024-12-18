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
