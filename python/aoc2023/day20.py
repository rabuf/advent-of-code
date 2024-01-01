import re
import sys
from collections import defaultdict
from enum import IntEnum
from math import lcm
from pathlib import Path

from aoc_util import print_day


class State(IntEnum):
    OFF = 0
    ON = 1


class Pulse(IntEnum):
    LOW = 0
    HIGH = 1


class PulseModule:
    def __init__(self, name, destinations):
        self.name = name
        self.destinations = destinations
        self.sources = []
        self.watch = False

    def add_source(self, source):
        if source not in self.sources:
            self.sources.append(source)

    def apply(self, origin, pulse):
        raise NotImplementedError


class SinkModule(PulseModule):
    def __init__(self, name):
        super().__init__(name, [])
        self.received = []

    def apply(self, origin, pulse):
        self.received.append(pulse)
        return []


class Broadcast(PulseModule):
    def apply(self, origin, pulse):
        return [(destination, self.name, pulse) for destination in self.destinations]


class FlipFlop(PulseModule):
    def __init__(self, name, destinations):
        super().__init__(name, destinations)
        self._state = State.OFF

    def apply(self, origin, pulse):
        match pulse:
            case Pulse.LOW:
                self._state = State.ON - self._state
                return [(destination, self.name, Pulse.LOW if self._state == State.OFF else Pulse.HIGH)
                        for destination in self.destinations]
            case Pulse.HIGH:
                return []


class Conjunction(PulseModule):
    def __init__(self, name, destinations):
        super().__init__(name, destinations)
        self.received = {}
        self.sent_high = False

    def add_source(self, source):
        super().add_source(source)
        self.received[source] = Pulse.LOW

    def apply(self, origin, pulse):
        self.received[origin] = pulse
        result = Pulse.LOW if all(self.received.values()) else Pulse.HIGH
        if result == Pulse.HIGH:
            self.sent_high = True
        return [(destination, self.name, result) for destination in self.destinations]


def parse_line(line):
    names = re.findall(r'\w+', line)
    name = names[0]
    destinations = names[1:]
    if name == "broadcaster":
        return Broadcast(name, destinations)
    if line[0] == '&':
        return Conjunction(name, destinations)
    if line[0] == '%':
        return FlipFlop(name, destinations)
    raise ValueError(f"Invalid line {line}")


def process_module_list(modules: list[PulseModule]):
    d = {}
    for m in modules:
        d[m.name] = m
    for m in modules:
        for destination in m.destinations:
            if destination not in d:
                d[destination] = SinkModule(destination)
            d[destination].add_source(m.name)
    return d


def lowest_presses(modules: dict[str, PulseModule], sink):
    cycles = defaultdict(int)
    sink_module = modules[sink]
    sources = sink_module.sources
    presses = 0
    while not all(cycles[source] for source in sources):
        presses += 1
        press_button(modules)
        for source in sources:
            if modules[source].sent_high and not cycles[source]:
                cycles[source] = presses
                modules[source].sent_high = False
    return lcm(*cycles.values())


def press_button(modules: dict[str, PulseModule], times=1):
    low, high = 0, 0
    for n in range(times):
        pulses = modules["broadcaster"].apply('button', Pulse.LOW)
        low += 1
        while pulses:
            destination, source, pulse = pulses.pop(0)
            low += pulse == Pulse.LOW
            high += pulse == Pulse.HIGH
            pulses.extend(modules[destination].apply(source, pulse))
    return low, high


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "20.txt") as f:
        raw = f.read().splitlines()
        modules = list(map(parse_line, raw))
        p1_modules = process_module_list(modules)
        p2_modules = process_module_list(list(map(parse_line, raw)))
        low, high = press_button(p1_modules, times=1000)
        last_conjunction = p2_modules['rx'].sources[0]
        cycles = lowest_presses(p2_modules, last_conjunction)
        print_day(20, low * high, cycles)


if __name__ == '__main__':
    main()
