import sys
from collections import Counter
from enum import IntEnum
from pathlib import Path

from aoc_util import print_day


def parse_line(line):
    hand, bid = line.split()
    bid = int(bid)
    return hand, hand_to_kind(hand), bid


class HandKind(IntEnum):
    HighCard = 1
    OnePair = 2
    TwoPair = 3
    ThreeOfAKind = 4
    FullHouse = 5
    FourOfAKind = 6
    FiveOfAKind = 7


def hand_to_kind(hand) -> HandKind:
    result = Counter(hand)
    match sorted(result.values()):
        case [5]:
            return HandKind.FiveOfAKind
        case [1, 4]:
            return HandKind.FourOfAKind
        case [2, 3]:
            return HandKind.FullHouse
        case [1, 1, 3]:
            return HandKind.ThreeOfAKind
        case [1, 2, 2]:
            return HandKind.TwoPair
        case [1, 1, 1, 2]:
            return HandKind.OnePair
        case _:
            return HandKind.HighCard


def hand_to_kind_joker(hand) -> HandKind:
    best = HandKind.HighCard
    for card in "23456789TQKA":
        alternate = hand.replace("J", card)
        kind = hand_to_kind(alternate)
        best = max(kind, best)
    return best


def card_to_value(card: str):
    if card.isnumeric():
        return int(card)
    return {"T": 10, "J": 11, "Q": 12, "K": 13, "A": 14}[card]


def compare_cards(c1: str, c2: str):
    if card_to_value(c1) < card_to_value(c2):
        return -1
    if c1 == c2:
        return 0
    return 1


def hand_to_number(hand: str) -> int:
    result = 0
    for card in hand:
        result = card_to_value(card) + result * 15
    return result


def score_hand(hands):
    hands = sorted(hands, key=lambda hand: hand_to_number(hand[0]))
    hands = sorted(hands, key=lambda hand: hand[1])
    scores = [bid * pos for pos, (_, _, bid) in enumerate(hands, start=1)]
    score = sum(scores)
    return score


def main(input_dir=Path(sys.argv[1])):
    with open(input_dir / "2023" / "07.txt") as f:
        hands = list(map(parse_line, f.read().splitlines()))
        score = score_hand(hands)
        joker_hands = [
            (hand.replace("J", "1"), hand_to_kind_joker(hand), bid)
            for (hand, _, bid) in hands
        ]
        joker_score = score_hand(joker_hands)
        print_day(7, score, joker_score)


if __name__ == "__main__":
    main()
