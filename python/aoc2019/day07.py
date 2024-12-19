import sys
import threading
from itertools import permutations
from pathlib import Path
from queue import Queue

from aoc2019 import intcode
from aoc_util import print_day


def run_vm(program, in_queue: Queue, out_queue: Queue):
    return threading.Thread(target=intcode.v2, args=(program,),
                            kwargs={'read': lambda: in_queue.get(), 'write': lambda n: out_queue.put(n)})


def thruster_controls(program, r):
    result = 0
    for perm in permutations(r):
        queues = [Queue() for _ in range(5)]
        threads = []
        for i in range(5):
            threads.append(run_vm(program, queues[i], queues[(i + 1) % 5]))
            threads[i].start()
            queues[i].put(perm[i], block=False)
        queues[0].put(0)
        for t in threads:
            t.join()
        result = max(result, queues[0].get())
    return result


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "07.txt") as f:
            program = intcode.parse_program(f.read())
        p1 = thruster_controls(program, range(5))
        p2 = thruster_controls(program, range(5, 10))
        print_day("07", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
