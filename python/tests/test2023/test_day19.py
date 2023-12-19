from aoc2023.day19 import *

sample = """px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"""


def test_parse_category():
    assert parse_part("{x=787,m=2655,a=1222,s=2876}") == {"x": 787, "m": 2655, "a": 1222, "s": 2876}


def test_part1():
    workflows, parts = sample.split('\n\n')
    workflows = dict(map(parse_workflow, workflows.splitlines()))
    parts = list(map(parse_part, parts.splitlines()))
    accepted = sum(apply_workflows(part, workflows) for part in parts)
    assert accepted == 19114


def test_part2():
    workflows, _ = sample.split('\n\n')
    workflows = dict(map(parse_workflow_range, workflows.splitlines()))
    accepted = apply_workflows_range({'x': (1, 4000), 'm': (1, 4000), 'a': (1, 4000), 's': (1, 4000)}, workflows)
    assert accepted == 167409079868000
