import sys
from collections import defaultdict
from functools import reduce
from pathlib import Path
from operator import lt, gt, mul
import regex as re
from aoc_util import print_day


def parse_condition(condition):
    category, operator, value = re.match(r'([amsx])([<>])(\d+)', condition).groups(0)
    value = int(value)
    match operator:
        case '<':
            return lambda part: lt(part[category], value)
        case '>':
            return lambda part: gt(part[category], value)


def parse_step(step):
    parts = step.split(':')
    if len(parts) == 2:
        condition = parse_condition(parts[0])
        to = parts[1]
        return condition, to
    else:
        return step


def parse_workflow(workflow):
    name, body = workflow.split('{')
    body = body[:-1]
    steps = list(map(parse_step, body.split(',')))
    return name, steps


def parse_part(part: str):
    categories = [c.split('=') for c in part.strip('{}').split(',')]
    return {category: int(value) for category, value in categories}


def apply_workflow(part, workflow):
    for step in workflow:
        match step:
            case condition, to:
                if condition(part):
                    return to
            case to:
                return to


def apply_workflows(part, workflows, initial='in'):
    state = initial
    while state not in 'AR':
        state = apply_workflow(part, workflows[state])
    return (state == 'A') * sum(part.values())


def parse_condition_range(condition):
    category, operator, value = re.match(r'([amsx])([<>])(\d+)', condition).groups(0)
    value = int(value)

    def less_than(part):
        lower, upper = part[category]
        if lower < value < upper:
            return {**part, category: (lower, value - 1)}, {**part, category: (value, upper)}
        if upper < value:
            return part, None
        return None, part

    def greater_than(part):
        lower, upper = part[category]
        if lower < value < upper:
            return {**part, category: (value + 1, upper)}, {**part, category: (lower, value)}
        if upper < value:
            return None, part
        return part, None

    match operator:
        case '<':
            return less_than
        case '>':
            return greater_than


def parse_step_range(step):
    parts = step.split(':')
    if len(parts) == 2:
        condition = parse_condition_range(parts[0])
        to = parts[1]
        return condition, to
    else:
        return step


def parse_workflow_range(workflow):
    name, body = workflow.split('{')
    body = body[:-1]
    steps = list(map(parse_step_range, body.split(',')))
    return name, steps


def apply_workflow_range(part, workflow):
    result = defaultdict(list)
    remaining = {**part}
    for step in workflow:
        match step:
            case condition, to:
                move, remaining = condition(remaining)
                if move and to != 'R':
                    result[to].append(move)
            case to:
                if remaining and to != 'R':
                    result[to].append(remaining)
        if not remaining:
            break
    return result


def apply_workflows_range(part, workflows):
    search_space = [('in', part)]
    result = 0
    while search_space:
        state, part = search_space.pop()
        next_spaces = apply_workflow_range(part, workflows[state])
        if 'A' in next_spaces:
            result += sum(reduce(mul, (upper - lower + 1 for lower, upper in part.values()))
                          for part in next_spaces['A'])
        search_space.extend((to, part) for to, parts in next_spaces.items() if to != 'A'
                            for part in parts)
    return result


def main():
    input_dir = Path(sys.argv[1])
    with open(input_dir / "2023" / "19.txt") as f:
        raw_workflows, raw_parts = f.read().split('\n\n')
    workflows = dict(map(parse_workflow, raw_workflows.splitlines()))
    parts = list(map(parse_part, raw_parts.splitlines()))
    accepted = sum(apply_workflows(part, workflows) for part in parts)
    range_workflows = dict(map(parse_workflow_range, raw_workflows.splitlines()))
    p2_accepted = apply_workflows_range({'x': (1, 4000), 'm': (1, 4000), 'a': (1, 4000), 's': (1, 4000)},
                                        range_workflows)
    print_day(19, accepted, p2_accepted)


if __name__ == '__main__':
    main()
