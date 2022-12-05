#!/usr/bin/env python3
from typing import List, Tuple
from pathlib import Path
import re

TEST = Path("test.txt").read_text()
INPUT = Path("input.txt").read_text()

def parseConf(conf: str) -> List[str]:
    return [line[1::4] for line in conf.splitlines()[:-1]]

def parseMoves(moves: str) -> List[Tuple[int, int, int]]:
    return [tuple(map(int, re.match("move (\d+) from (\d+) to (\d+)", line).groups()))
            for line in moves.splitlines()]

def parse(input: str):
    conf, moves = input.split("\n\n")
    return (parseConf(conf), parseMoves(moves))

def initial(conf: List[str]):
    stacks = [[] for _ in range(len(conf[0]))] # depends on the correct trailing whitespace
    for line in reversed(conf):
        for (idx, crate) in enumerate(line):
            if crate.strip():
                stacks[idx].append(crate)
    return stacks

def move(stacks, mv):
    times, fr, to = mv
    for _ in range(times):
        crate = stacks[fr-1].pop()
        stacks[to-1].append(crate)

def readout(stacks):
    return "".join(([ stack[-1] for stack in stacks ]))

def part1(input):
    conf, moves = parse(input)
    stacks = initial(conf)
    for mv in moves:
        move(stacks, mv)
    return readout(stacks)

def multimove(stacks, mv):
    times, fr, to = mv
    stack = stacks[fr-1]
    idx = len(stack) - times
    rest, crates = stack[:idx], stack[idx:]
    stacks[fr-1] = rest
    stacks[to-1] += crates

def part2(input):
    conf, moves = parse(input)
    stacks = initial(conf)
    for mv in moves:
        multimove(stacks, mv)
    return readout(stacks)

print(f"Part1 answer: {part1(INPUT)}")
print(f"Part2 answer: {part2(INPUT)}")
