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


def main():
    input_dir = Path(sys.argv[1])
    try:
        with open(input_dir / "2019" / "07.txt") as f:
            program = intcode.parse_program(f.read())
        p1 = 0
        for perm in permutations(range(5)):
            queues = [Queue() for _ in range(6)]
            threads = []
            for i in range(5):
                threads.append(run_vm(program, queues[i], queues[i + 1]))
                threads[i].start()
                queues[i].put(perm[i], block=False)
            queues[0].put(0)
            for t in threads:
                t.join()
            p1 = max(p1, queues[5].get())
        p2 = 0
        for perm in permutations(range(5, 10)):
            queues = [Queue() for _ in range(5)]
            threads = []
            for i in range(5):
                threads.append(run_vm(program, queues[i], queues[(i + 1) % 5]))
                threads[i].start()
                queues[i].put(perm[i], block=False)
            queues[0].put(0)
            for t in threads:
                t.join()
            p2 = max(p2, queues[0].get())
        print_day("07", p1, p2)
    except IOError as e:
        print(e)


if __name__ == '__main__':
    main()
