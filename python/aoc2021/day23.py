import sys
from pathlib import Path
from queue import PriorityQueue

from aoc_util import print_day


def lines_to_state(lines, depth=2):
    rooms = [[lines[i][x] for i in range(depth + 1, 1, -1)] for x in [3, 5, 7, 9]]
    return "  | | | |  ", *rooms


def copy_state(state):
    return tuple(part.copy() for part in state)


COST = {"A": 1, "B": 10, "C": 100, "D": 1000}
HOME = {"A": 0, "B": 1, "C": 2, "D": 3}
ROOM_DONE = {
    2: [["A", "A"], ["B", "B"], ["C", "C"], ["D", "D"]],
    4: [["A"] * 4, ["B"] * 4, ["C"] * 4, ["D"] * 4],
}
END_STATE = {2: ("  | | | |  ", *ROOM_DONE[2]), 4: ("  | | | |  ", *ROOM_DONE[4])}


def heuristic(state):
    estimate = 0
    for i, c in enumerate(state[0]):
        if c in HOME:
            target = HOME[c] * 2 + 2
            distance = abs(i - target) + 1
            estimate += COST[c] * distance
    return estimate


def to_room_moves(state, depth=2):
    result = []
    hallway = state[0]
    rooms = state[1:]
    for i, amphipod in enumerate(hallway):
        # empty spot in hallway
        if amphipod not in "ABCD":
            continue
        next_rooms = [r.copy() for r in rooms]
        home_room = next_rooms[HOME[amphipod]]
        # home not clear
        if any(a != amphipod for a in home_room):
            continue
        home_column = HOME[amphipod] * 2 + 2
        # amphipod right of home
        if i > home_column:
            path = hallway[home_column:i]
        else:
            path = hallway[i + 1 : home_column + 1]
        # Not a clear path
        if any(c in "ABCD" for c in path):
            continue
        cost = COST[amphipod] * (len(path) + depth - len(home_room))
        next_hallway = hallway[:i] + " " + hallway[i + 1 :]
        home_room.append(amphipod)
        next_state = (cost, (next_hallway, *next_rooms))
        result.append(next_state)
    return result


def to_hallway_moves(state, depth=2):
    result = []
    for i, room in enumerate(state[1:]):
        x = 2 * i + 2  # where it meets the hallway
        # All the amphipods are in their home, don't move any
        if all(HOME[room[n]] == i for n in range(len(room))):
            continue
        hallway = state[0]
        next_rooms = [r.copy() for r in state[1:]]
        amphipod = next_rooms[i].pop()
        for next_x in range(x + 1, len(hallway)):
            # Hallway blocked from this point
            if hallway[next_x] in "ABCD":
                break
            if hallway[next_x] == " ":
                next_hall = hallway[:next_x] + amphipod + hallway[next_x + 1 :]
                cost = COST[amphipod] * (next_x - x + 1 + depth - len(room))
                result.append((cost, (next_hall, *next_rooms)))
        for next_x in range(x, -1, -1):
            if hallway[next_x] in "ABCD":
                break
            if hallway[next_x] == " ":
                next_hall = hallway[:next_x] + amphipod + hallway[next_x + 1 :]
                cost = COST[amphipod] * (x - next_x + 1 + depth - len(room))
                result.append((cost, (next_hall, *next_rooms)))
    return result


def next_states(state, depth=2):
    return to_hallway_moves(state, depth) + to_room_moves(state, depth)


def settify_state(state):
    return state[0], *[tuple(r) for r in state[1:]]


def search(initial_state, depth=2):
    q = PriorityQueue()
    q.put((0, 0, initial_state))
    visited = set()
    while q.not_empty:
        (_, cost, state) = q.get()
        if state == END_STATE[depth]:
            return cost
        states = next_states(state, depth)
        for c, s in states:
            if settify_state(s) not in visited:
                visited.add(settify_state(s))
                q.put((c + cost + heuristic(s), c + cost, s))


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2021" / "23.txt") as f:
        lines = f.read().splitlines()
        unfolded = lines[0:3] + ["  #D#C#B#A#", "  #D#B#A#C#"] + lines[3:]
        p1_state = lines_to_state(lines, depth=2)
        p1 = search(p1_state)
        p2_state = lines_to_state(unfolded, depth=4)
        p2 = search(p2_state, depth=4)
        print_day(23, p1, p2)


if __name__ == "__main__":
    main()
