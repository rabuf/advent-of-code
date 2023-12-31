import re
import sys
from enum import IntEnum
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
        self._sources = []

    def add_source(self, source):
        self._sources.append(source)

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
        self._received = {}

    def add_source(self, source):
        super().add_source(source)
        self._received[source] = Pulse.LOW

    def apply(self, origin, pulse):
        self._received[origin] = pulse
        result = Pulse.LOW if pulse and all(self._received.values()) else Pulse.HIGH
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


def lowest_presses(modules: dict[str, PulseModule], sink='rx'):
    presses = 0
    while all(modules[sink].received):
        modules[sink].received = []
        press_button(modules)
        presses += 1
    return presses


def press_button(modules: dict[str, PulseModule], times=1):
    low, high = 0, 0
    for _ in range(times):
        pulses = modules["broadcaster"].apply('button', Pulse.LOW)
        low += 1
        while pulses:
            destination, source, pulse = pulses.pop()
            low += pulse == Pulse.LOW
            high += pulse == Pulse.HIGH
            pulses.extend(modules[destination].apply(source, pulse))
    return low, high


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "20.txt") as f:
        modules = list(map(parse_line, f.read().splitlines()))
        p1_modules = process_module_list(modules)
        p2_modules = process_module_list(modules)
        low, high = press_button(p1_modules, times=1000)
        presses = 0  # lowest_presses(p2_modules)
        print_day(20, low * high, presses)


if __name__ == '__main__':
    main()
